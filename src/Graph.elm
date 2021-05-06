module Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Angle
import Axis3d
import Dict exposing (Dict)
import Direction3d
import Element as E exposing (..)
import Element.Input as I
import Length exposing (Length, inMeters, meters)
import List.Extra as List
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Set exposing (Set)
import TrackEditType exposing (..)
import TrackPoint exposing (..)
import Utils exposing (showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


type Direction
    = Forwards
    | Backwards


type alias XY =
    ( Float, Float )


type alias EdgeKey =
    ( XY, XY, XY )


type alias Graph =
    { nodes : Dict XY TrackPoint
    , edges : Dict EdgeKey (List TrackPoint)
    , route : List Traversal
    , centreLineOffset : Length
    , trackPointToCanonical : Dict XY PointType
    , nodePairsInRoute : List ( Int, Int )
    }


type
    PointType
    -- We shall use this to build an index back from Trackpoint land to Graph land.
    = NodePoint XY -- Canonical index of node
    | EdgePoint EdgeKey -- Canonical index of edge, canonical index of node


emptyGraph : Graph
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , route = []
    , centreLineOffset = Length.meters 0.0
    , trackPointToCanonical = Dict.empty
    , nodePairsInRoute = []
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset Float
    | ApplyOffset
    | ConvertFromGraph


type GraphActionImpact
    = GraphCreated
    | GraphOffsetChange
    | GraphOffsetApplied
    | GraphNoAction
    | GraphRemoved


type alias Traversal =
    { edge : EdgeKey -- Canonical index of edge
    , direction : Direction
    }


type alias Route =
    { route : List Traversal }


info =
    """## Graph

This is a whole new mode of working with routes. It divides the track
into "Edges" (these can be traversed more than once, in either direction),
and "Nodes" (these are where Edges join).

In the process, elevation differences resulting from route planning tools
are removed, so there is a _canonical_ form for each Edge. All your future
work will use these canonical edges for consistent elevation.

Once this is done, you can apply up to 5m horizontal separation between
different edge traversal directions. Choose left or right to suit your
local traffic convention (usually).

More to follow ...
"""


viewGraphControls : (Msg -> msg) -> Maybe Graph -> Element msg
viewGraphControls wrapper graph =
    let
        offset =
            Maybe.map .centreLineOffset graph
                |> Maybe.map inMeters
                |> Maybe.withDefault 0.0

        analyseButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper GraphAnalyse)
                , label = E.text "Convert to Graph"
                }

        finishButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper ConvertFromGraph)
                , label = E.text "Convert from Graph"
                }

        offsetSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << CentreLineOffset
                , label =
                    I.labelBelow [] <|
                        E.text <|
                            "Offset = "
                                ++ (showDecimal2 <| abs offset)
                                ++ "m "
                                ++ (if offset < 0.0 then
                                        "left"

                                    else if offset > 0.0 then
                                        "right"

                                    else
                                        ""
                                   )
                , min = -5.0
                , max = 5.0
                , step = Just 1.0
                , value = offset
                , thumb = I.defaultThumb
                }

        applyOffsetButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper ApplyOffset)
                , label = E.text "Apply offset"
                }
    in
    if graph == Nothing then
        E.el [ width fill, paddingXY 20 10 ] analyseButton

    else
        E.row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
            [ E.column [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                [ offsetSlider
                --, applyOffsetButton
                ]
            , finishButton
            ]


update :
    Msg
    -> List TrackPoint
    -> Maybe Graph
    -> ( Maybe Graph, GraphActionImpact )
update msg trackPoints graph =
    case msg of
        GraphAnalyse ->
            ( Just <| deriveTrackPointGraph trackPoints
            , GraphCreated
            )

        CentreLineOffset offset ->
            case graph of
                Just isGraph ->
                    ( Just { isGraph | centreLineOffset = meters offset }
                    , GraphOffsetChange
                    )

                Nothing ->
                    ( Nothing, GraphNoAction )

        ApplyOffset ->
            ( graph, GraphOffsetApplied )

        ConvertFromGraph ->
            ( Nothing, GraphRemoved )


deriveTrackPointGraph : List TrackPoint -> Graph
deriveTrackPointGraph trackPoints =
    let
        --trackPoints =
        --    -- Might help to avoid false nodes.
        --    List.filter (\tp -> tp.costMetric > 0) unfilteredTrackPoints
        rawNodes =
            interestingTrackPoints trackPoints

        rawEdges =
            findDistinctEdges rawNodes trackPoints

        nodePairs : List ( Int, Int )
        nodePairs =
            rawEdges
                |> List.map
                    (\raw ->
                        let
                            ( fst, lst ) =
                                ( List.head raw, List.last raw )
                        in
                        Maybe.map2 (\n1 n2 -> ( n1.index, n2.index )) fst lst
                    )
                |> List.filterMap identity

        canonicalEdges =
            findCanonicalEdges rawEdges

        canonicalRoute : List Traversal
        canonicalRoute =
            useCanonicalEdges rawEdges canonicalEdges
                |> List.map
                    (\( edgeKey, direction ) ->
                        { edge = edgeKey
                        , direction = direction
                        }
                    )

        isPointColinear : EdgeKey -> Bool
        isPointColinear ( start, end, via ) =
            -- Our working definition is if the two triangle legs
            -- are no more than 2m longer than the straight line.
            let
                ( a, c, b ) =
                    ( Point2d.fromTuple meters start
                    , Point2d.fromTuple meters end
                    , Point2d.fromTuple meters via
                    )

                ( ac, bc, ab ) =
                    ( Point2d.distanceFrom a c
                    , Point2d.distanceFrom b c
                    , Point2d.distanceFrom a b
                    )
            in
            Quantity.plus ab bc
                |> Quantity.lessThan
                    (Quantity.plus ac (Length.meters 2.0))

        singlePointLinearEdges : Dict EdgeKey (List TrackPoint)
        singlePointLinearEdges =
            -- Edges with one waypoint sometimes result from manually placed track points
            -- during the route planning. Removing these can result in avoiding the formation
            -- of spurious nodes either side (the extra point fools the neighbour count).
            -- We spot that if there is only one point, it must be the waypoint.
            canonicalEdges
                |> Dict.filter
                    (\edgeKey edge ->
                        List.length edge
                            == 1
                            && isPointColinear edgeKey
                    )

        annoyingTrackPoints : List XY
        annoyingTrackPoints =
            singlePointLinearEdges
                |> Dict.keys
                |> List.map (\( start, end, via ) -> via)

        removeAnnoyingPoints =
            List.filterNot
                (\tp -> List.member (trackPointComparable tp) annoyingTrackPoints)
                trackPoints

        graph =
            { emptyGraph
                | nodes = rawNodes
                , edges = canonicalEdges
                , route = canonicalRoute
                , nodePairsInRoute = nodePairs
            }
    in
    case annoyingTrackPoints of
        [] ->
            graph

        _ ->
            deriveTrackPointGraph removeAnnoyingPoints


walkTheRouteInternal : Graph -> List ( TrackPoint, PointType )
walkTheRouteInternal graph =
    -- This will convert the original route into a route made from canonical edges.
    let
        addToTrail { edge, direction } accumulator =
            let
                ( startXY, endXY, viaXY ) =
                    edge

                edgePoints =
                    Dict.get edge graph.edges |> Maybe.withDefault []

                edgeStart =
                    Dict.get startXY graph.nodes

                edgeEnd =
                    Dict.get endXY graph.nodes
            in
            case ( edgeStart, edgeEnd, direction ) of
                ( Just startTP, Just endTP, Forwards ) ->
                    { accumulator
                        | points =
                            ( startTP, NodePoint startXY )
                                :: addEdgePoints edge edgePoints
                                ++ [ ( endTP, NodePoint endXY ) ]
                                ++ List.drop 1 accumulator.points
                    }

                ( Just startTP, Just endTP, Backwards ) ->
                    { accumulator
                        | points =
                            ( endTP, NodePoint endXY )
                                :: (List.reverse <| addEdgePoints edge edgePoints)
                                ++ [ ( startTP, NodePoint startXY ) ]
                                ++ List.drop 1 accumulator.points
                    }

                _ ->
                    accumulator

        addEdgePoints : ( XY, XY, XY ) -> List TrackPoint -> List ( TrackPoint, PointType )
        addEdgePoints edgeId edgePoints =
            List.map
                (\pt -> ( pt, EdgePoint edgeId ))
                edgePoints
    in
    List.foldr
        addToTrail
        { points = [], nextIdx = 0 }
        graph.route
        |> .points


trackPointComparable : TrackPoint -> XY
trackPointComparable tp =
    -- An important part of the graph is to remove height differences on repeated passes.
    let
        ( x, y, _ ) =
            Point3d.toTuple inMeters tp.xyz
    in
    ( x, y )


endPoints tps =
    -- Subtle point: if the start and end coincide we want the start point to "win".
    List.take 1 (List.reverse tps) ++ List.take 1 tps


addPointsFromList listPoints dict =
    List.foldl
        (\tp d -> Dict.insert (trackPointComparable tp) tp d)
        dict
        listPoints


interestingTrackPoints : List TrackPoint -> Dict XY TrackPoint
interestingTrackPoints tps =
    let
        neighbourMap =
            neighbourMapHelper Dict.empty tps

        neighbourMapHelper dict tp =
            case tp of
                t0 :: t1 :: t2 :: tRest ->
                    -- "t0 and t2 are neighbours of t1"
                    neighbourMapHelper (addNeighbours dict t0 t1 t2) (t1 :: t2 :: tRest)

                [ ty, tz ] ->
                    -- "ty is the only neighbour for tz"
                    addOneNeighbourAtEnd dict ty tz

                _ ->
                    dict

        addNeighbours dict t0 t1 t2 =
            -- "t0 and t2 are neighbours of t1"
            let
                key =
                    trackPointComparable t1

                current =
                    Dict.get key dict

                ( left, right ) =
                    ( trackPointComparable t0, trackPointComparable t2 )
            in
            Dict.insert key
                (case current of
                    Just ( tp, neighbours ) ->
                        -- Use whichever track point we first saw at this location.
                        ( tp, Set.insert left <| Set.insert right neighbours )

                    Nothing ->
                        -- t1 becomes the examplar track point at this location.
                        ( t1, Set.insert left <| Set.insert right Set.empty )
                )
                dict

        addOneNeighbourAtEnd dict ty tz =
            let
                key =
                    trackPointComparable tz

                current =
                    Dict.get key dict

                left =
                    trackPointComparable ty
            in
            Dict.insert key
                (case current of
                    Just ( tp, neighbours ) ->
                        ( tp, Set.insert left neighbours )

                    Nothing ->
                        ( tz, Set.insert left Set.empty )
                )
                dict

        notTwoNeighbours _ ( _, neighbours ) =
            -- Three or more is a junction.
            -- Need to add S/F explicitly
            Set.size neighbours /= 2

        interesting =
            Dict.filter notTwoNeighbours neighbourMap
    in
    -- We don't need to return the neighbour list.
    interesting
        |> Dict.map (\k ( tp, _ ) -> tp)
        |> addPointsFromList (endPoints tps)


findDistinctEdges :
    Dict XY TrackPoint
    -> List TrackPoint
    -> List (List TrackPoint)
findDistinctEdges nodes trackPoints =
    -- I probably should develop this into what I really want, but it's quite neat
    -- and clear, so I think I'll make another pass to substitute the canonical edges.
    let
        atNode : TrackPoint -> Bool
        atNode tp =
            Dict.member (trackPointComparable tp) nodes

        routeSplitter :
            TrackPoint -- The start point of an edge == a junction or the route start
            -> List (List TrackPoint) -- Accumulator for fold.
            -> List TrackPoint -- The list being folded.
            -> List (List TrackPoint) -- The result list, in the natural order.
        routeSplitter previousNode edges tps =
            -- Not quite what we want as the node to appear on both edges; here it's on the departing edge.
            case List.splitWhen atNode tps of
                Just ( foundEdge, foundNextNode :: nextEdge ) ->
                    -- We borrow the first node of the next edge here for our edge, and pass it forwards.
                    routeSplitter
                        foundNextNode
                        ((previousNode :: foundEdge ++ [ foundNextNode ]) :: edges)
                        nextEdge

                Just ( before, _ ) ->
                    -- Last edge, so just prepend the carried forward node.
                    (previousNode :: before) :: edges

                Nothing ->
                    -- Reverse list so it's in the natural order.
                    edges |> List.reverse

        edgeList : List (List TrackPoint)
        edgeList =
            case trackPoints of
                p0 :: ps ->
                    routeSplitter p0 [] trackPoints

                _ ->
                    []
    in
    -- First edge (as coded) will just contain the start node.
    List.drop 1 edgeList


findCanonicalEdges :
    List (List TrackPoint)
    -> Dict EdgeKey (List TrackPoint)
findCanonicalEdges originalEdges =
    -- Note we are keying on three coordinates, so we disambiguate edges between node pairs.
    List.foldl addCanonical Dict.empty originalEdges


addCanonical :
    List TrackPoint
    -> Dict EdgeKey (List TrackPoint)
    -> Dict EdgeKey (List TrackPoint)
addCanonical edge dict =
    let
        edgeLength =
            List.length edge

        ( startNode, secondNode ) =
            ( List.head edge
            , List.getAt 1 edge
            )

        ( finishNode, penultimateNode ) =
            ( List.last edge
            , List.getAt (edgeLength - 2) edge
            )

        forwardVia =
            case secondNode of
                Just _ ->
                    secondNode

                Nothing ->
                    finishNode

        reverseVia =
            case penultimateNode of
                Just _ ->
                    penultimateNode

                Nothing ->
                    startNode

        edgePoints =
            edge |> List.take (edgeLength - 1) |> List.drop 1
    in
    case [ startNode, forwardVia, reverseVia, finishNode ] of
        [ Just isStart, Just isForwardVia, Just isReverseVia, Just isFinish ] ->
            let
                ( comp1, comp2 ) =
                    ( trackPointComparable isStart
                    , trackPointComparable isForwardVia
                    )

                ( compM, compN ) =
                    ( trackPointComparable isReverseVia
                    , trackPointComparable isFinish
                    )

                ( forwardKey, reverseKey ) =
                    ( ( comp1, compN, comp2 )
                    , ( compN, comp1, compM )
                    )

                forwardEntryFound =
                    Dict.member forwardKey dict

                reverseEntryFound =
                    Dict.member reverseKey dict
            in
            if
                -- We may have encountered in either direction.
                forwardEntryFound || reverseEntryFound
            then
                -- Previously encountered.
                dict

            else
                -- First encounter for this edge, so this is canonical.
                Dict.insert forwardKey edgePoints dict

        _ ->
            dict


useCanonicalEdges :
    List (List TrackPoint)
    -> Dict EdgeKey (List TrackPoint)
    -> List ( EdgeKey, Direction )
useCanonicalEdges edges canonicalEdges =
    let
        replaceEdge : List TrackPoint -> Maybe ( EdgeKey, Direction )
        replaceEdge edge =
            let
                edgeLength =
                    List.length edge

                ( startNode, secondNode ) =
                    ( List.head edge
                    , List.getAt 1 edge
                    )

                ( finishNode, penultimateNode ) =
                    ( List.last edge
                    , List.getAt (edgeLength - 2) edge
                    )

                forwardVia =
                    case secondNode of
                        Just _ ->
                            secondNode

                        Nothing ->
                            finishNode

                reverseVia =
                    case penultimateNode of
                        Just _ ->
                            penultimateNode

                        Nothing ->
                            startNode
            in
            case [ startNode, forwardVia, reverseVia, finishNode ] of
                [ Just isStart, Just isForwardVia, Just isReverseVia, Just isFinish ] ->
                    let
                        ( comp1, comp2 ) =
                            ( trackPointComparable isStart
                            , trackPointComparable isForwardVia
                            )

                        ( compM, compN ) =
                            ( trackPointComparable isReverseVia
                            , trackPointComparable isFinish
                            )

                        ( forwardKey, reverseKey ) =
                            ( ( comp1, compN, comp2 )
                            , ( compN, comp1, compM )
                            )

                        forwardEntryFound =
                            Dict.get forwardKey canonicalEdges

                        reverseEntryFound =
                            Dict.get reverseKey canonicalEdges
                    in
                    case ( forwardEntryFound, reverseEntryFound ) of
                        ( Just _, _ ) ->
                            Just ( ( comp1, compN, comp2 ), Forwards )

                        ( Nothing, Just _ ) ->
                            Just ( ( compN, comp1, compM ), Backwards )

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    List.map replaceEdge edges |> List.filterMap identity


walkTheRoute : Graph -> List TrackPoint
walkTheRoute graph =
    let
        walkedRoute =
            walkTheRouteInternal graph
    in
    walkedRoute
        |> List.map Tuple.first
        |> prepareTrackPoints
        |> List.map (applyCentreLineOffset graph.centreLineOffset)


applyCentreLineOffset : Length -> TrackPoint -> TrackPoint
applyCentreLineOffset offset trackpoint =
    let
        offsetDirection =
            trackpoint.effectiveDirection
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

        offsetVector =
            Vector3d.withLength offset offsetDirection

        newXYZ =
            Point3d.translateBy offsetVector trackpoint.xyz
    in
    { trackpoint | xyz = newXYZ }


nodePointList : Graph -> List (Point3d Length.Meters LocalCoords)
nodePointList graph =
    let
        whereTheNodesAre =
            graph.nodes
                |> Dict.values
                |> List.map .xyz
    in
    whereTheNodesAre


applyIndexPreservingEditsToGraph : ( Int, Int ) -> List TrackPoint -> Graph -> Graph
applyIndexPreservingEditsToGraph ( editStart, editEnd ) newTrackPoints graph =
    -- Using our knowledge of the route (Edges and Directions) and node pairs
    -- we can apply posution-only changes to our canonical nodes and edges.
    let
        routeInfoPairs =
            List.zip graph.nodePairsInRoute graph.route

        edgesOverlappingRange =
            routeInfoPairs
                |> List.filter
                    (\( ( edgeStart, edgeEnd ), { edge, direction } ) ->
                        edgeStart <= editEnd && edgeEnd >= editStart
                    )

        ( updatedNodes, updatedEdges ) =
            edgesOverlappingRange
                |> List.foldl
                    applyPositionsFromNewEdge
                    ( graph.nodes, graph.edges )

        applyPositionsFromNewEdge ( ( edgeStart, edgeEnd ), { edge, direction } ) ( nodes, edges ) =
            let
                ( fromXY, toXY, viaXY ) =
                    edge

                trackpointsOnEdgeIncludingNodes =
                    newTrackPoints |> List.take edgeEnd |> List.drop edgeStart

                trackpointsOnEdge =
                    trackpointsOnEdgeIncludingNodes
                        |> List.take (List.length trackpointsOnEdgeIncludingNodes - 1)
                        |> List.drop 1

                ( startNodeTrackPoint, endNodeTrackPoint ) =
                    ( List.head trackpointsOnEdgeIncludingNodes
                    , List.last trackpointsOnEdgeIncludingNodes
                    )

                orientedEdge =
                    case direction of
                        Forwards ->
                            trackpointsOnEdge

                        Backwards ->
                            List.reverse trackpointsOnEdge
            in
            ( case ( startNodeTrackPoint, endNodeTrackPoint ) of
                ( Just isStart, Just isEnd ) ->
                    nodes
                        |> Dict.insert fromXY isStart
                        |> Dict.insert toXY isEnd

                _ ->
                    nodes
            , edges |> Dict.insert edge orientedEdge
            )
    in
    { graph | nodes = updatedNodes, edges = updatedEdges }


applyNodePreservingEditsToGraph : ( Int, Int ) -> List TrackPoint -> Graph -> Graph
applyNodePreservingEditsToGraph ( editStart, editEnd ) newPoints graph =
    let
        routeInfoPairs =
            List.zip graph.nodePairsInRoute graph.route

        edgeContainingingRange =
            -- Our rules state there must be only one edge for this type of edit.
            routeInfoPairs
                |> List.find
                    (\( ( edgeStart, edgeEnd ), _ ) ->
                        editStart
                            >= edgeStart
                            && editStart
                            <= edgeEnd
                            && editEnd
                            >= edgeStart
                            && editEnd
                            <= edgeEnd
                    )
    in
    case edgeContainingingRange of
        Just ( ( edgeStart, edgeEnd ), { edge, direction } ) ->
            -- We now have to find the extent of the new edge by finding the next Node.
            -- We know only where the edit ends; there could be more point beyond it.
            let
                ( regionBeforeEditEnd, regionAfterEditEnd ) =
                    List.splitAt editEnd newPoints

                ( startXY, endXY, _ ) =
                    edge

                isNotNode point =
                    trackPointComparable point
                        /= startXY
                        && trackPointComparable point
                        /= endXY

                leadingPartOfNewEdge =
                    regionBeforeEditEnd |> List.takeWhileRight isNotNode

                trailingPartOfNewEdge =
                    regionAfterEditEnd |> List.takeWhile isNotNode

                resultingNewEdgePoints =
                    leadingPartOfNewEdge ++ trailingPartOfNewEdge

                orientedEdge =
                    if direction == Forwards then
                        resultingNewEdgePoints

                    else
                        List.reverse resultingNewEdgePoints

                newEdgeDict =
                    graph.edges |> Dict.insert edge orientedEdge
            in
            { graph | edges = newEdgeDict }

        Nothing ->
            graph


updateWithNewTrack :
    Maybe Graph
    -> List TrackPoint
    -> ( Int, Int )
    -> List TrackPoint
    -> TrackEditType
    -> Maybe Graph
updateWithNewTrack oldGraph oldTrack editRegion newTrack editType =
    case oldGraph of
        Nothing ->
            -- Dispense with trivial case first.
            oldGraph

        Just graph ->
            let
                newGraph =
                    case editType of
                        EditPreservesIndex ->
                            -- E.g. Nudge, may span multiple Edges and can update Nodes.
                            applyIndexPreservingEditsToGraph editRegion newTrack graph

                        EditPreservesNodePosition ->
                            -- E.g. Insert/Delete, can only work on one edge excluding nodes.
                            applyNodePreservingEditsToGraph editRegion newTrack graph

                        EditNoOp ->
                            -- Used only by Undo/Redo; no graph change.
                            graph
            in
            Just graph
