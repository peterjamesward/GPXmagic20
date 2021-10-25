module Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Angle
import Arc3d
import Axis3d
import BezierSplines exposing (bezierSplines)
import ColourPalette
import Dict exposing (Dict)
import Dict.Extra
import Direction2d
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as I
import GeometryShared exposing (arc3dFromThreePoints)
import Length exposing (Length, Meters, inFeet, inMeters, meters)
import LineSegment3d
import List.Extra as List
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import SketchPlane3d
import TrackEditType exposing (..)
import TrackPoint exposing (..)
import Triangle3d
import Utils exposing (showDecimal2)
import Vector2d
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
    , userRoute : List Traversal
    , canonicalRoute : List Traversal
    , centreLineOffset : Length.Length
    , trackPointToCanonical : Dict XY PointType

    --, nodePairsInRoute : List ( Int, Int )
    , selectedTraversal : Maybe Traversal
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
    , userRoute = []
    , canonicalRoute = []
    , centreLineOffset = Length.meters 0.0
    , trackPointToCanonical = Dict.empty

    --, nodePairsInRoute = []
    , selectedTraversal = Nothing
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset Float
    | ConvertFromGraph
    | HighlightTraversal Traversal
    | RemoveLastTraversal
    | AddTraversalFromCurrent
    | AddFirstTraversal


type GraphActionImpact
    = GraphCreated
    | GraphOffsetChange
    | GraphRouteChanged
    | GraphNoAction
    | GraphRemoved
    | GraphShowTraversal


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

Click "Show" on any segment to highlight. Click "Remove ..." to take segments off
the **end** of the list. Click "Add ..." to add an adjacent edge where the Orange
pointer is.

Once this is done, you can apply up to 5m horizontal separation between
different edge traversal directions. Choose left or right to suit your
local traffic convention (usually).

Click "Convert from..." to render your new route for a final polish.

**NOTE** This will not work well, if at all, with recorded rides.
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
                , label = text "Convert to Graph"
                }

        finishButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper ConvertFromGraph)
                , label = text "Convert from Graph"
                }

        offsetSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << CentreLineOffset
                , label =
                    I.labelBelow [] <|
                        text <|
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

        removeButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper RemoveLastTraversal)
                , label = text "Remove traversal\nlast in list"
                }

        addButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper AddTraversalFromCurrent)
                , label = text "Add traversal\nat Orange marker"
                }

        addFirstButton =
            I.button prettyButtonStyles
                { onPress = Just (wrapper AddFirstTraversal)
                , label = text "Add traversal using\nPurple and Orange markers"
                }
    in
    case graph of
        Nothing ->
            el [ width fill, paddingXY 20 10 ] analyseButton

        Just g ->
            column [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                [ row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                    [ offsetSlider
                    , finishButton
                    ]
                , showTheRoute g wrapper
                , row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ] <|
                    if List.length g.userRoute > 0 then
                        [ removeButton
                        , addButton
                        ]

                    else
                        [ addFirstButton ]
                ]


showTheRoute : Graph -> (Msg -> msg) -> Element msg
showTheRoute graph wrap =
    let
        traversalData =
            List.filterMap showTraversal graph.userRoute

        showTraversal t =
            let
                ( from, to, ( viaX, viaY ) ) =
                    t.edge

                ( node1, node2 ) =
                    ( Dict.get from graph.nodes
                    , Dict.get to graph.nodes
                    )
            in
            case ( node1, node2 ) of
                ( Just n1, Just n2 ) ->
                    Just
                        { direction =
                            case t.direction of
                                Forwards ->
                                    "Forwards "

                                Backwards ->
                                    "Backwards "
                        , from = n1.index
                        , to = n2.index
                        , link = t
                        }

                _ ->
                    Nothing

        headerAttrs =
            [ Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            ]

        makeShowButton : Traversal -> Element msg
        makeShowButton t =
            I.button
                [ Background.color <|
                    if Just t == graph.selectedTraversal then
                        ColourPalette.radioButtonSelected

                    else
                        ColourPalette.accordionContentBackground
                ]
                { onPress = Just (wrap <| HighlightTraversal t)
                , label = text "Show"
                }
    in
    column
        [ width <| maximum 500 fill
        , height <| px 250
        , spacing 10
        , padding 5
        , Border.width 2
        , Border.rounded 6
        , Border.color ColourPalette.expandedTabBorder
        ]
        [ row [ width fill ]
            [ el ((width <| fillPortion 3) :: headerAttrs) <| text "Direction"
            , el ((width <| fillPortion 1) :: headerAttrs) <| text "From"
            , el ((width <| fillPortion 1) :: headerAttrs) <| text "To"
            , el ((width <| fillPortion 2) :: headerAttrs) <| text "Show"
            ]

        -- workaround for a bug: it's necessary to wrap `table` in an `el`
        -- to get table height attribute to apply
        , el [ width fill ] <|
            table
                [ width fill
                , height <| px 200
                , scrollbarY
                , spacing 10
                ]
                { data = traversalData
                , columns =
                    [ { header = none
                      , width = fillPortion 3
                      , view = .direction >> text >> el [ centerY ]
                      }
                    , { header = none
                      , width = fillPortion 1
                      , view = .from >> String.fromInt >> text
                      }
                    , { header = none
                      , width = fillPortion 1
                      , view = .to >> String.fromInt >> text
                      }
                    , { header = none
                      , width = fillPortion 2
                      , view = .link >> makeShowButton
                      }
                    ]
                }
        ]


update :
    Msg
    -> List TrackPoint
    -> TrackPoint -- orange marker needed to add traversal.
    -> Maybe TrackPoint -- purple marker needed for first traversal only
    -> Maybe Graph
    -> ( Maybe Graph, GraphActionImpact )
update msg trackPoints current purple graph =
    case ( msg, graph ) of
        ( GraphAnalyse, _ ) ->
            ( Just <| deriveTrackPointGraph trackPoints
            , GraphCreated
            )

        ( CentreLineOffset offset, Just isGraph ) ->
            ( Just { isGraph | centreLineOffset = meters offset }
            , GraphOffsetChange
            )

        ( ConvertFromGraph, _ ) ->
            ( graph, GraphRemoved )

        ( HighlightTraversal t, Just isGraph ) ->
            ( Just { isGraph | selectedTraversal = Just t }
            , GraphShowTraversal
            )

        ( RemoveLastTraversal, Just isGraph ) ->
            let
                entries =
                    List.length isGraph.userRoute

                selectedIndex =
                    Maybe.withDefault 0 <|
                        case isGraph.selectedTraversal of
                            Just selected ->
                                List.findIndex ((==) selected) isGraph.userRoute

                            Nothing ->
                                Just 0
            in
            ( Just
                { isGraph
                    | userRoute = List.take (entries - 1) isGraph.userRoute
                    , selectedTraversal =
                        if selectedIndex == entries - 1 then
                            Nothing

                        else
                            isGraph.selectedTraversal
                }
            , GraphRouteChanged
            )

        ( AddTraversalFromCurrent, Just isGraph ) ->
            addTraversalFromCurrent isGraph current

        ( AddFirstTraversal, Just isGraph ) ->
            addFirstTraversal isGraph current purple

        _ ->
            ( graph, GraphNoAction )


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
                , userRoute = canonicalRoute
                , canonicalRoute = canonicalRoute

                --, nodePairsInRoute = nodePairs
            }
    in
    case annoyingTrackPoints of
        [] ->
            graph

        _ ->
            deriveTrackPointGraph removeAnnoyingPoints


walkTheRouteInternal : Graph -> List Traversal -> List ( TrackPoint, PointType )
walkTheRouteInternal graph route =
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
        route
        |> .points


nodePairsFromRoute : Graph -> List ( Int, Int )
nodePairsFromRoute graph =
    -- This list is used to locate an edited track section by comparing the track point indices
    -- with the addresses of edge start and end pairs.
    -- Whilst it makes sense to keep the node pair list in the graph, we then have
    -- the burden of ensuring it is up to date. But it's quite cheap to derive so
    -- that feels (in retrospect) like premature optimisation.
    let
        endNodes =
            List.scanl
                (\traversal ->
                    case Dict.get traversal.edge graph.edges of
                        Just edge ->
                            (+) (1 + List.length edge)

                        Nothing ->
                            (+) 0
                )
                0
                graph.canonicalRoute
    in
    List.zip endNodes (List.drop 1 endNodes)


trackPointComparable : TrackPoint -> XY
trackPointComparable tp =
    -- An important part of the graph is to remove height differences on repeated passes.
    let
        ( x, y, _ ) =
            Point3d.toTuple inFeet tp.xyz

        ( xRounded, yRounded ) =
            ( toFloat <| round x, toFloat <| round y )
    in
    ( xRounded, yRounded )


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
    walkTheRouteInternal graph graph.canonicalRoute
        |> List.map Tuple.first


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
            List.zip (nodePairsFromRoute graph) graph.canonicalRoute

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
                    newTrackPoints |> List.take (edgeEnd + 1) |> List.drop edgeStart

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
            List.zip (nodePairsFromRoute graph) graph.canonicalRoute

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

                ( regionBeforeEditStart, editedRegion ) =
                    List.splitAt editStart regionBeforeEditEnd

                ( startXY, endXY, _ ) =
                    edge

                isNotNode point =
                    trackPointComparable point
                        /= startXY
                        && trackPointComparable point
                        /= endXY

                leadingPartOfNewEdge =
                    regionBeforeEditStart |> List.takeWhileRight isNotNode

                trailingPartOfNewEdge =
                    regionAfterEditEnd |> List.takeWhile isNotNode

                resultingNewEdgePoints =
                    leadingPartOfNewEdge ++ editedRegion ++ trailingPartOfNewEdge

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
            Just newGraph


previewTraversal : Graph -> List TrackPoint
previewTraversal graph =
    case graph.selectedTraversal of
        Just traversal ->
            let
                ( start, end, via ) =
                    traversal.edge

                ( startTP, endTP ) =
                    ( Dict.get start graph.nodes, Dict.get end graph.nodes )

                edge =
                    Dict.get traversal.edge graph.edges |> Maybe.withDefault []
            in
            case ( startTP, endTP ) of
                ( Just node1, Just node2 ) ->
                    if traversal.direction == Forwards then
                        node1 :: edge ++ [ node2 ]

                    else
                        List.reverse <| node1 :: edge ++ [ node2 ]

                _ ->
                    []

        Nothing ->
            []


addTraversalFromCurrent : Graph -> TrackPoint -> ( Maybe Graph, GraphActionImpact )
addTraversalFromCurrent graph current =
    -- Figure out which edge the current track point is on.
    -- If it's accessible from the end of the current route, add it to the route.
    let
        edgeContainingCurrent =
            graph.edges
                |> Dict.Extra.find
                    (\edgeKey points ->
                        points
                            |> List.find
                                (\p -> p.index == current.index)
                            |> (/=) Nothing
                    )

        lastTraversalOfRoute =
            List.last graph.userRoute
    in
    case ( edgeContainingCurrent, lastTraversalOfRoute ) of
        ( Just ( newEdgeKey, _ ), Just lastTraversal ) ->
            let
                ( lastStart, lastEnd, _ ) =
                    lastTraversal.edge

                lastNode =
                    if lastTraversal.direction == Forwards then
                        lastEnd

                    else
                        lastStart

                ( newStart, newEnd, _ ) =
                    newEdgeKey

                newSegment =
                    if newStart == lastNode then
                        Just { edge = newEdgeKey, direction = Forwards }

                    else if newEnd == lastNode then
                        Just { edge = newEdgeKey, direction = Backwards }

                    else
                        Nothing

                newRoute =
                    graph.userRoute ++ Maybe.Extra.toList newSegment
            in
            ( Just
                { graph
                    | userRoute = newRoute
                    , selectedTraversal = newSegment
                }
            , GraphRouteChanged
            )

        _ ->
            let
                _ =
                    Debug.log "Edge not found for" current
            in
            ( Just graph, GraphNoAction )


addFirstTraversal : Graph -> TrackPoint -> Maybe TrackPoint -> ( Maybe Graph, GraphActionImpact )
addFirstTraversal graph orange purple =
    -- For the first section, we use the Purple as an "anchor" and Orange to
    -- indicate the direction of travel.
    let
        edgeContainingOrange =
            graph.edges
                |> Dict.Extra.find
                    (\edgeKey points -> List.member orange points)

        edgeContainingPurple =
            case purple of
                Just isPurple ->
                    graph.edges
                        |> Dict.Extra.find
                            (\edgeKey points -> List.member isPurple points)

                Nothing ->
                    Nothing
    in
    case ( edgeContainingOrange, edgeContainingPurple, purple ) of
        ( Just ( newEdgeKey, pointsOnEdge ), Just ( purpleEdgeKey, _ ), Just isPurple ) ->
            if newEdgeKey == purpleEdgeKey then
                let
                    ( orangeIndex, purpleIndex ) =
                        ( List.findIndex
                            (.xyz >> Point3d.equalWithin (meters 0.01) orange.xyz)
                            pointsOnEdge
                        , List.findIndex
                            (.xyz >> Point3d.equalWithin (meters 0.01) isPurple.xyz)
                            pointsOnEdge
                        )

                    newSegment =
                        case ( orangeIndex, purpleIndex ) of
                            ( Just fromHere, Just toHere ) ->
                                if toHere >= fromHere then
                                    Just { edge = newEdgeKey, direction = Forwards }

                                else
                                    Just { edge = newEdgeKey, direction = Backwards }

                            _ ->
                                Nothing
                in
                ( Just
                    { graph
                        | userRoute = Maybe.Extra.toList newSegment
                        , selectedTraversal = newSegment
                    }
                , GraphRouteChanged
                )

            else
                ( Just graph, GraphNoAction )

        _ ->
            ( Just graph, GraphNoAction )


publishUserRoute : Graph -> List TrackPoint
publishUserRoute graph =
    let
        skeleton =
            skeletalRoute graph
    in
    List.concat <|
        List.map3
            (routeStepRenderer <| inMeters graph.centreLineOffset)
            skeleton
            (List.drop 1 skeleton)
            (List.drop 2 skeleton)



-- This is the trivial version with no corrections at node transitions.
--walkTheRouteInternal graph graph.userRoute
--    |> List.unzip
--    |> Tuple.first
--    |> prepareTrackPoints
--    |> List.map (applyCentreLineOffset graph.centreLineOffset)


type RouteRenderingHint
    = RouteStart
    | RouteNode TrackPoint
    | RouteEdge (List TrackPoint)
    | RouteEnd
    | RouteError -- Something not in its Dictionary. Bad.


skeletalRoute : Graph -> List RouteRenderingHint
skeletalRoute graph =
    -- First of two phases. Here we simplify the structure by de-referencing all the dict lookups.
    let
        startNodeFromFirstTraversal : RouteRenderingHint
        startNodeFromFirstTraversal =
            graph.userRoute
                |> List.head
                |> Maybe.map startNodeFromTraversal
                |> Maybe.withDefault RouteError

        startNodeFromTraversal : Traversal -> RouteRenderingHint
        startNodeFromTraversal { edge, direction } =
            let
                ( start, end, _ ) =
                    edge
            in
            graph.nodes
                |> Dict.get
                    (if direction == Forwards then
                        start

                     else
                        end
                    )
                |> Maybe.map RouteNode
                |> Maybe.withDefault RouteError

        endNodeFromTraversal : Traversal -> RouteRenderingHint
        endNodeFromTraversal { edge, direction } =
            let
                ( start1, end1, _ ) =
                    edge
            in
            graph.nodes
                |> Dict.get
                    (if direction == Forwards then
                        end1

                     else
                        start1
                    )
                |> Maybe.map RouteNode
                |> Maybe.withDefault RouteError

        orientEdge : Traversal -> List TrackPoint -> List TrackPoint
        orientEdge t e =
            if t.direction == Forwards then
                e

            else
                List.reverse e

        edgeAndEndNode : Traversal -> List RouteRenderingHint
        edgeAndEndNode t =
            let
                ( start, end, via ) =
                    t.edge

                edgePoints =
                    Dict.get t.edge graph.edges
            in
            [ edgePoints |> Maybe.map (orientEdge t) |> Maybe.withDefault [] |> RouteEdge
            , endNodeFromTraversal t
            ]
    in
    List.concat
        [ [ RouteStart ]
        , [ startNodeFromFirstTraversal ]
        , List.concatMap edgeAndEndNode graph.userRoute
        , [ RouteEnd ]
        ]


routeStepRenderer : Float -> RouteRenderingHint -> RouteRenderingHint -> RouteRenderingHint -> List TrackPoint
routeStepRenderer offset prev current next =
    let
        closerThan distance point1 point2 =
            Point3d.distanceFrom point1.xyz point2.xyz
                |> Quantity.lessThanOrEqualTo (meters <| abs distance)
    in
    -- Second phase. We consume the list "three at a time" so we always have context for the nodes.
    case ( prev, current, next ) of
        ( RouteStart, RouteNode node, RouteEdge outgoing ) ->
            -- Just emit the start node, offset from vector of first road segment
            let
                firstPointOnEdge =
                    List.head outgoing

                shifted =
                    Maybe.map (applyOffsetToFirstPoint offset node) firstPointOnEdge
                        |> Maybe.withDefault node
            in
            [ shifted ]

        ( RouteNode start, RouteEdge edge, RouteNode end ) ->
            --Edge render must exclude points within offset zones11
            let
                properPoints =
                    edge
                        |> List.dropWhileRight (closerThan offset end)
                        |> List.dropWhile (closerThan offset start)
            in
            -- The edge itself is easy
            List.map2
                (applyOffsetToFirstPoint offset)
                properPoints
                (List.drop 1 properPoints ++ [ end ])

        ( RouteEdge incoming, RouteNode node, RouteEdge outgoing ) ->
            -- All depends on the angles and the offset
            -- We should now be able to discard any points closer than the offset.
            -- This will give more predicatability.
            let
                ( incomingBeyondOffset, outgoingBeyondOffset ) =
                    ( incoming |> List.dropWhileRight (closerThan offset node)
                    , outgoing |> List.dropWhile (closerThan offset node)
                    )

                ( precedingPoint, followingPoint ) =
                    ( List.last incomingBeyondOffset |> Maybe.withDefault node
                    , List.head outgoingBeyondOffset |> Maybe.withDefault node
                    )

                ( inVector, outVector ) =
                    ( Vector3d.from precedingPoint.xyz node.xyz
                    , Vector3d.from node.xyz followingPoint.xyz
                    )

                ( inDirection, outDirection ) =
                    ( Vector3d.projectInto SketchPlane3d.xy inVector
                        |> Vector2d.direction
                        |> Maybe.withDefault Direction2d.positiveX
                    , Vector3d.projectInto SketchPlane3d.xy outVector
                        |> Vector2d.direction
                        |> Maybe.withDefault Direction2d.positiveX
                    )

                ( shiftedPriorPoint, shiftedNextPoint ) =
                    ( applyOffsetToFirstPoint offset precedingPoint node
                    , applyOffsetToSecondPoint offset node followingPoint
                    )

                ( shiftedNodeIncoming, shiftedNodeOutgoing ) =
                    -- Only for U-turn.
                    ( applyOffsetToSecondPoint offset precedingPoint node
                    , applyOffsetToFirstPoint offset node followingPoint
                    )

                amountOfTurn =
                    Direction2d.angleFrom inDirection outDirection

                mitreAngle =
                    if amountOfTurn |> Quantity.greaterThanOrEqualTo zero then
                        Angle.degrees 180 |> Quantity.minus amountOfTurn |> Quantity.divideBy 2.0

                    else
                        Angle.degrees -180 |> Quantity.minus amountOfTurn |> Quantity.divideBy 2.0

                insideMitreDirection =
                    outDirection |> Direction2d.rotateBy mitreAngle

                outsideMitreDirection =
                    Direction2d.reverse insideMitreDirection

                ( insideMitreVector, outsideMitreVector ) =
                    ( Vector2d.withLength (meters <| abs offset) insideMitreDirection
                        |> Vector3d.on SketchPlane3d.xy
                    , Vector2d.withLength (meters <| abs offset) outsideMitreDirection
                        |> Vector3d.on SketchPlane3d.xy
                    )

                ( insideMitrePoint, outsideMitrePoint ) =
                    ( node.xyz |> Point3d.translateBy insideMitreVector |> trackPointFromPoint
                    , node.xyz |> Point3d.translateBy outsideMitreVector |> trackPointFromPoint
                    )

                centroidWithMitrePoint =
                    Triangle3d.fromVertices ( shiftedPriorPoint.xyz, insideMitrePoint.xyz, shiftedNextPoint.xyz )
                        |> Triangle3d.centroid
                        |> trackPointFromPoint

                --_ =
                --    Debug.log "Mitre vector" insideMitreVector
            in
            -- Offset right is +ve.
            -- Left turn is +ve.
            -- U-turn is +pi (but would not rule out -pi).
            if offset == 0.0 then
                -- No need to apply offset but still worth applying some smoothing,
                if Quantity.abs amountOfTurn |> Quantity.lessThanOrEqualTo (Angle.degrees 10) then
                    --let
                    --    _ =
                    --        Debug.log "Branch A" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    [ node ]

                else
                    --let
                    --    _ =
                    --        Debug.log "Branch B" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    generateSmoothedTurn shiftedPriorPoint node shiftedNextPoint

            else if
                (amountOfTurn |> Quantity.greaterThanOrEqualTo zero)
                    && (amountOfTurn |> Quantity.lessThanOrEqualTo (Angle.degrees 10))
            then
                -- Small turn to the left, use the appropriate mitre point
                if offset < 0.0 then
                    --let
                    --    _ =
                    --        Debug.log "Branch C" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    [ insideMitrePoint ]

                else
                    --let
                    --    _ =
                    --        Debug.log "Branch D" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    [ outsideMitrePoint ]

            else if
                (amountOfTurn |> Quantity.greaterThan (Angle.degrees 10))
                    && (amountOfTurn |> Quantity.lessThan (Angle.degrees 170))
            then
                -- Large turn to left
                if offset < 0.0 then
                    -- Offset is on inside of turn
                    --let
                    --    _ =
                    --        Debug.log "Branch E" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    --generateSmoothedTurn shiftedPriorPoint insideMitrePoint shiftedNextPoint
                    -- Empiricall, this Bezier through the centroid looks nice. IMHO.
                    bezierSplines
                        False
                        0.5
                        1.0
                        [ shiftedPriorPoint, centroidWithMitrePoint, shiftedNextPoint ]

                else
                    -- Insert an arc around the offset zone.
                    --let
                    --    _ =
                    --        Debug.log "Branch F" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    generateArc shiftedNodeIncoming outsideMitrePoint shiftedNodeOutgoing

            else if
                (amountOfTurn |> Quantity.lessThan zero)
                    && (amountOfTurn |> Quantity.greaterThanOrEqualTo (Angle.degrees -10))
            then
                -- Small turn to the right, use the appropriate mitre point
                if offset > 0.0 then
                    --let
                    --    _ =
                    --        Debug.log "Branch G" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    [ insideMitrePoint ]

                else
                    --let
                    --    _ =
                    --        Debug.log "Branch H" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    [ outsideMitrePoint ]

            else if
                (amountOfTurn |> Quantity.lessThan (Angle.degrees -10))
                    && (amountOfTurn |> Quantity.greaterThan (Angle.degrees -170))
            then
                -- Large turn to the right
                if offset > 0.0 then
                    -- Offset is on inside of turn
                    --let
                    --    _ =
                    --        Debug.log "Branch I" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    bezierSplines
                        False
                        0.5
                        1.0
                        [ shiftedPriorPoint, centroidWithMitrePoint, shiftedNextPoint ]

                else
                    -- Insert an arc around the offset zone.
                    --let
                    --    _ =
                    --        Debug.log "Branch J" ( Angle.inDegrees amountOfTurn, offset )
                    --in
                    generateArc shiftedNodeIncoming outsideMitrePoint shiftedNodeOutgoing

            else
                -- it's a U-turn
                --let
                --    _ =
                --        Debug.log "Branch K" ( Angle.inDegrees amountOfTurn, offset )
                --in
                generateArc shiftedNodeIncoming outsideMitrePoint shiftedNodeOutgoing

        ( RouteEdge incoming, RouteNode node, RouteEnd ) ->
            -- Just emit the final node offset from vector of final segment
            let
                lastPointOnEdge =
                    List.last incoming

                shifted =
                    case lastPointOnEdge of
                        Just penultimate ->
                            applyOffsetToSecondPoint offset penultimate node

                        Nothing ->
                            node
            in
            [ shifted ]

        _ ->
            -- Anything else means something has gone wrong
            []


generateArc : TrackPoint -> TrackPoint -> TrackPoint -> List TrackPoint
generateArc entry middle exit =
    -- Quick and simple "rounding" effect on midpoint
    -- Can only use when curve is passing through these points, hence OUTSIDE bend.
    case Arc3d.throughPoints entry.xyz middle.xyz exit.xyz of
        Just arc ->
            arc
                |> Arc3d.segments 6
                |> Polyline3d.segments
                |> List.map LineSegment3d.startPoint
                |> List.drop 1
                |> List.map trackPointFromPoint

        Nothing ->
            []


generateSmoothedTurn : TrackPoint -> TrackPoint -> TrackPoint -> List TrackPoint
generateSmoothedTurn entry middle exit =
    -- Re-purpose the 3D smoother; not required to pass through all three points,
    -- they provide the envelope.
    case arc3dFromThreePoints entry middle exit of
        Just arc ->
            Arc3d.startPoint arc
                :: (Arc3d.segments 5 arc
                        |> Polyline3d.segments
                        |> List.map LineSegment3d.endPoint
                   )
                |> List.map trackPointFromPoint

        Nothing ->
            []


applyOffsetToFirstPoint : Float -> TrackPoint -> TrackPoint -> TrackPoint
applyOffsetToFirstPoint offset pointToOffset nextPoint =
    let
        offsetDirection =
            Vector3d.from pointToOffset.xyz nextPoint.xyz
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

        offsetVector =
            Vector3d.withLength (meters offset) offsetDirection

        newXYZ =
            Point3d.translateBy offsetVector pointToOffset.xyz
    in
    { pointToOffset | xyz = newXYZ }


applyOffsetToSecondPoint : Float -> TrackPoint -> TrackPoint -> TrackPoint
applyOffsetToSecondPoint offset prevPoint pointToOffset =
    let
        offsetDirection =
            Vector3d.from prevPoint.xyz pointToOffset.xyz
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

        offsetVector =
            Vector3d.withLength (meters offset) offsetDirection

        newXYZ =
            Point3d.translateBy offsetVector pointToOffset.xyz
    in
    { pointToOffset | xyz = newXYZ }



-- END
