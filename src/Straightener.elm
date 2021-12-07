module Straightener exposing (..)

import Color
import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (inMeters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (EditResult, UndoEntry)
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0)
import ViewPureStyles exposing (prettyButtonStyles)


toolLabel =
    "Straighten"


info =
    """## Straightener

Use this when you want a really straight piece of road.

It will preserve elevations, so it might result in some
undesirable local gradients.

Use it also when your original track has weird bits that
go backwards, if these were not cleansed on loading.

There's also the option to progressively remove track points
that contribute least to the shape of the track (defined by the
area of the triangle of each point and its
neightbours, since you asked). This can be useful when you
start with an IRL ride, as these contain a lot of noise."""


type Msg
    = SimplifyTrack
    | StraightenStraight
    | SearchForSimplifications


type alias Options =
    { metricFilteredPoints : List TrackPoint }


defaultOptions =
    { metricFilteredPoints = [] }


type alias StraightenInfo =
    -- No real need to put the 'after' values in here.
    -- Just an easier code change to make.
    { start : Int
    , end : Int
    , before : List (Point3d.Point3d Length.Meters LocalCoords)
    , after : List (Point3d.Point3d Length.Meters LocalCoords)
    }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        SearchForSimplifications ->
            ( lookForSimplifications settings track
            , PostUpdateActions.ActionPreview
            )

        StraightenStraight ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildStraightenActions settings track)
            )

        SimplifyTrack ->
            ( { settings | metricFilteredPoints = [] }
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildSimplifyActions settings track)
            )


viewStraightenTools : Options -> (Msg -> msg) -> Track -> Element msg
viewStraightenTools options wrapper track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        searchButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper SearchForSimplifications
                , label =
                    text <|
                        "Look for simplifications"
                }

        simplifyButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper SimplifyTrack
                , label =
                    text <|
                        "Remove "
                            ++ (String.fromInt <| List.length options.metricFilteredPoints)
                            ++ "\ntrack points"
                }

        straightenButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper StraightenStraight
                , label =
                    text <|
                        "Straighten between markers"
                }
    in
    wrappedRow [ spacing 10, padding 10 ]
        [ if track.currentNode /= marker then
            straightenButton

          else
            paragraph [ padding 10 ]
                [ text "The straighten tool requires a range. "
                , text "Drop the marker and move it away from the current pointer."
                ]
        , if List.length options.metricFilteredPoints > 0 then
            simplifyButton

          else
            searchButton
        , paragraph [ padding 10 ]
            [ text "Simplify works across a range if available,"
            , text " otherwise the whole track."
            ]
        ]


buildStraightenActions : Options -> Track -> UndoEntry
buildStraightenActions options track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            ( if track.currentNode.index <= marker.index then
                track.currentNode

              else
                marker
            , if track.currentNode.index > marker.index then
                track.currentNode

              else
                marker
            )

        undoMessage =
            "Straighten from "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " to "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        idealLine =
            LineSegment3d.from startPoint.xyz endPoint.xyz

        ( xAtStart, xLength ) =
            ( inMeters startPoint.distanceFromStart
            , inMeters endPoint.distanceFromStart
                - inMeters startPoint.distanceFromStart
            )

        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt startPoint.index

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (endPoint.index - startPoint.index)

        applyAdjustment : TrackPoint -> Point3d.Point3d Length.Meters LocalCoords
        applyAdjustment pt =
            let
                current =
                    Point3d.toRecord inMeters pt.xyz

                straightenedPoint =
                    LineSegment3d.interpolate idealLine
                        ((inMeters pt.distanceFromStart - xAtStart) / xLength)
                        |> Point3d.toRecord inMeters
            in
            Point3d.fromRecord meters { straightenedPoint | z = current.z }

        undoRedoInfo : StraightenInfo
        undoRedoInfo =
            { start = startPoint.index
            , end = endPoint.index
            , before = region |> List.map .xyz
            , after = region |> List.map applyAdjustment
            }
    in
    { label = undoMessage
    , editFunction = applyStraighten undoRedoInfo
    , undoFunction = undoStraighten undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


applyStraighten : StraightenInfo -> Track -> EditResult
applyStraighten undoRedoInfo track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.start

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (undoRedoInfo.end - undoRedoInfo.start)

        adjusted =
            -- Make it so.
            List.map2
                (\pt new -> { pt | xyz = new })
                region
                undoRedoInfo.after
    in
    { before = prefix
    , edited = adjusted
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


undoStraighten : StraightenInfo -> Track -> EditResult
undoStraighten undoRedoInfo track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.start

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (undoRedoInfo.end - undoRedoInfo.start)

        adjusted =
            -- Make it so.
            List.map2
                (\pt new -> { pt | xyz = new })
                region
                undoRedoInfo.before
    in
    { before = prefix
    , edited = adjusted
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


type alias SimplifyInfo =
    -- Can this work?
    { start : Int
    , end : Int
    , nodesToRemove : List TrackPoint
    }


buildSimplifyActions : Options -> Track -> UndoEntry
buildSimplifyActions options track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.markedNode == Nothing then
                ( 0, List.length track.trackPoints - 1 )

            else if track.currentNode.index <= marker.index then
                ( track.currentNode.index, marker.index )

            else
                ( marker.index, track.currentNode.index )

        undoMessage =
            "Remove "
                ++ String.fromInt removeCount
                ++ " points"

        nodesToRemove =
            List.filter
                (\pt -> pt.index > startPoint && pt.index < endPoint)
                options.metricFilteredPoints

        removeCount =
            List.length nodesToRemove

        undoRedoInfo : SimplifyInfo
        undoRedoInfo =
            { start = startPoint
            , end = endPoint
            , nodesToRemove = nodesToRemove
            }
    in
    { label = undoMessage
    , editFunction = applySimplify undoRedoInfo
    , undoFunction = undoSimplify undoRedoInfo
    , newOrange = min 0 (track.currentNode.index - removeCount)
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


simplifyWithDefaults : Track -> List TrackPoint
simplifyWithDefaults track =
    -- Helper for One-Click-Quick-Fix
    let
        actions =
            buildSimplifyActions defaultOptions track

        results =
            actions.editFunction track
    in
    results.edited |> TrackPoint.prepareTrackPoints


applySimplify : SimplifyInfo -> Track -> EditResult
applySimplify undoRedoInfo track =
    -- Both input lists are sorted in index order.
    let
        idxsToRemove =
            List.map .index undoRedoInfo.nodesToRemove

        ( _, _, retained ) =
            helper idxsToRemove track.trackPoints []

        helper idxs tps kept =
            case ( idxs, tps ) of
                ( [], _ ) ->
                    ( [], [], List.reverse tps ++ kept )

                ( _, [] ) ->
                    ( [], [], kept )

                ( i :: is, t :: ts ) ->
                    if t.index == i then
                        helper is ts kept

                    else if t.index < i then
                        helper idxs ts (t :: kept)

                    else
                        -- t.idx > i
                        helper is tps kept

        --_ =
        --    Debug.log "APPLY" undoRedoInfo
    in
    { before = []
    , edited = List.reverse retained
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


type alias UndoFolder =
    { lastInserted : Int
    , fragments : List (List TrackPoint)
    }


undoSimplify : SimplifyInfo -> Track -> EditResult
undoSimplify undoRedoInfo track =
    -- We have a list of the track points that were removed.
    -- We need to put them back in their respective places.
    -- I think we can recurse down the two lists, much like apply.
    let
        startState : UndoFolder
        startState =
            { lastInserted = -1
            , fragments = []
            }

        showState state =
            "inserted "
                ++ String.fromInt state.lastInserted
                ++ ", fragments "
                ++ (String.concat <|
                        List.concat <|
                            List.map
                                (List.map
                                    (\pt -> String.fromInt pt.index ++ ",")
                                )
                                state.fragments
                   )

        helper : UndoFolder -> List TrackPoint -> List TrackPoint -> UndoFolder
        helper state toInsert source =
            case ( toInsert, source ) of
                ( [], _ ) ->
                    { state | fragments = source :: state.fragments }

                ( _, [] ) ->
                    { state | fragments = source :: state.fragments }

                ( nextInsert :: moreInserts, _ ) ->
                    let
                        nextSplit =
                            -- How many to consume before inserting.
                            nextInsert.index - state.lastInserted - 1

                        ( prefix, residue ) =
                            source |> List.Extra.splitAt nextSplit
                    in
                    helper
                        { lastInserted = nextInsert.index
                        , fragments = [ nextInsert ] :: prefix :: state.fragments
                        }
                        moreInserts
                        residue

        endState =
            helper startState undoRedoInfo.nodesToRemove track.trackPoints

        reconstruction =
            endState.fragments
                |> List.reverse
                |> List.concat
    in
    { before = []
    , edited = reconstruction
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


lookForSimplifications : Options -> Track -> Options
lookForSimplifications options track =
    let
        numberOfNodes =
            List.length track.trackPoints

        sortedByMetric =
            track.trackPoints
                |> List.take (numberOfNodes - 1)
                |> List.drop 1
                |> List.sortBy .costMetric

        fraction =
            --TODO: Expose this parameter to the user.
            0.2

        numberToRemove =
            truncate <| fraction * toFloat numberOfNodes

        selectionForRemoval =
            List.take numberToRemove sortedByMetric

        forRemovalInIndexOrder =
            List.sortBy .index selectionForRemoval

        avoidingNeighbours : Int -> List TrackPoint -> List TrackPoint
        avoidingNeighbours lastRemoved suggestions =
            -- Don't remove adjacent nodes
            case suggestions of
                [] ->
                    []

                pt1 :: pts ->
                    if pt1.index == lastRemoved + 1 then
                        -- remove it from the removal list, but not at either track end.
                        avoidingNeighbours lastRemoved pts

                    else
                        pt1 :: avoidingNeighbours pt1.index pts

        filteredPointIndices =
            avoidingNeighbours -1 forRemovalInIndexOrder
    in
    { options | metricFilteredPoints = filteredPointIndices }


getPreview3D : Options -> Track -> List (Entity LocalCoords)
getPreview3D options track =
    highlightPoints Color.lightRed options.metricFilteredPoints
