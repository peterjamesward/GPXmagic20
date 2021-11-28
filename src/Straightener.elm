module Straightener exposing (..)

import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (inMeters, meters)
import LineSegment3d
import Point3d
import PostUpdateActions
import Track exposing (Track)
import TrackEditType as PostUpdateActions
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


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        StraightenStraight ->
            let
                ( newTrack, undoMsg ) =
                    straightenStraight track
            in
            ( settings
            ,             PostUpdateActions.ActionNoOp
--PostUpdateActions.ActionTrackChanged
--                PostUpdateActions.EditPreservesIndex
--                newTrack
--                undoMsg
            )

        SimplifyTrack ->
            let
                ( newTrack, undoMsg ) =
                    simplifyTrack settings track
            in
            ( settings
            ,             PostUpdateActions.ActionNoOp
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


straightenStraight : Track -> ( Track, String )
straightenStraight track =
    -- In contrast to v1, we will just map over the whole track,
    -- ignoring anything outside the selected range.
    -- We will just interpolate (x,y) using the distance from
    -- start of range.
    -- Very similar to SmoothGradient but working on (xy) not (z).
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

        straightenWithinRegion =
            List.map applyAdjustment track.trackPoints

        applyAdjustment : TrackPoint -> TrackPoint
        applyAdjustment pt =
            -- This reads nicer than the v1 splicing and folding method.
            if pt.index > startPoint.index && pt.index < endPoint.index then
                let
                    current =
                        Point3d.toRecord inMeters pt.xyz

                    straightenedPoint =
                        LineSegment3d.interpolate idealLine
                            ((inMeters pt.distanceFromStart - xAtStart) / xLength)
                            |> Point3d.toRecord inMeters

                    newPoint =
                        Point3d.fromRecord meters { straightenedPoint | z = current.z }
                in
                { pt | xyz = newPoint }

            else
                pt
    in
    ( { track | trackPoints = straightenWithinRegion }
    , undoMessage
    )


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
