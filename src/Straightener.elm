module Straightener exposing (..)

import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (inMeters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (UndoEntry)
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


type alias Options =
    { metricFilteredPoints : List Int }


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
        StraightenStraight ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildStraightenActions settings track)
            )

        SimplifyTrack ->
            let
                ( newTrack, undoMsg ) =
                    simplifyTrack settings track
            in
            ( settings
            , PostUpdateActions.ActionNoOp
              --PostUpdateActions.ActionTrackChanged
              --                PostUpdateActions.EditPreservesNodePosition
              --                newTrack
              --                undoMsg
            )


viewStraightenTools : Options -> (Msg -> msg) -> Track -> Element msg
viewStraightenTools options wrapper track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        simplifyButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper SimplifyTrack
                , label =
                    text <|
                        "Remove up to "
                            ++ String.fromInt (List.length options.metricFilteredPoints)
                            ++ " track points\nto simplify the route."
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
        , simplifyButton
        , paragraph [ padding 10 ]
            [ text "Simplify works across a range if available,"
            , text " otherwise the whole track."
            ]
        ]


lookForSimplifications : Options -> Track -> Options
lookForSimplifications options track =
    let
        numberOfNodes =
            List.length track.trackPoints

        sortedByMetric =
            List.sortBy .costMetric track.trackPoints

        fraction =
            --TODO: Expose this parameter to the user.
            0.2

        numberToRemove =
            truncate <| fraction * toFloat numberOfNodes

        selectionForRemoval =
            List.take numberToRemove sortedByMetric

        forRemovalInIndexOrder =
            List.sort <|
                List.map
                    .index
                    selectionForRemoval

        avoidingNeighbours lastRemoved suggestions =
            -- Don't remove adjacent nodes
            case suggestions of
                [] ->
                    []

                [ n ] ->
                    [ n ]

                n1 :: ns ->
                    if n1 == lastRemoved + 1 then
                        -- remove it from the removal list.
                        avoidingNeighbours lastRemoved ns

                    else
                        n1 :: avoidingNeighbours n1 ns

        filteredPointIndices =
            avoidingNeighbours -1 forRemovalInIndexOrder
    in
    { options | metricFilteredPoints = filteredPointIndices }


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
    }

applyStraighten : StraightenInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
                (\pt new ->  { pt | xyz = new }  )
                region
                undoRedoInfo.after
    in
    ( prefix, adjusted, suffix )


undoStraighten : StraightenInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
                (\pt new ->  { pt | xyz = new }  )
                region
                undoRedoInfo.before
    in
    ( prefix, adjusted, suffix )

simplifyTrack : Options -> Track -> ( Track, String )
simplifyTrack options track =
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
                ++ String.fromInt (List.length nodesToRemove)
                ++ " track points"

        nodesToRemove =
            List.filter
                (\n -> n > startPoint && n < endPoint)
                options.metricFilteredPoints
    in
    ( { track | trackPoints = removeByIndexNumbers nodesToRemove track.trackPoints }
    , undoMessage
    )


removeByIndexNumbers : List Int -> List TrackPoint -> List TrackPoint
removeByIndexNumbers idxsToRemove trackPoints =
    -- Both input lists are sorted in index order.
    let
        ( _, _, retained ) =
            helper idxsToRemove trackPoints []

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
    in
    List.reverse retained
