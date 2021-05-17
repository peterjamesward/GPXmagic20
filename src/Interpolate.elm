module Interpolate exposing (..)

import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (inMeters)
import List.Extra
import Point3d
import PostUpdateActions
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (trackPointFromPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Insert

Often, with planned routes, track points are too widely spaced
to give the control you need near problem areas.

With this tool, you can insert track points so that they have a
guaranteed maximum spacing. You can then use other tools to form
the shapes you seek.

This is good to use before the Bend Smoother or the Centroid filter, 
for example."""


type Msg
    = SetMaxSpacing Float
    | InsertPoints


type alias Options =
    { maxSpacing : Float }


defaultOptions =
    { maxSpacing = 3.0 }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction msg)
update msg settings track =
    case msg of
        SetMaxSpacing spacing ->
            ( { settings | maxSpacing = spacing }
            , PostUpdateActions.ActionNoOp
            )

        InsertPoints ->
            let
                ( newTrack, undoMsg ) =
                    insertPoints settings track
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                newTrack
                undoMsg
            )


viewTools : Options -> (Msg -> msg) -> Element msg
viewTools options wrap =
    let
        splitSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrap << SetMaxSpacing
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Maximum gap "
                                ++ showDecimal2 options.maxSpacing
                                ++ "m"
                , min = 1.0
                , max = 50.0
                , step = Just 1.0
                , value = options.maxSpacing
                , thumb = Input.defaultThumb
                }
    in
    row [ spacing 10, padding 10 ]
        [ splitSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrap InsertPoints
            , label = text "Insert points"
            }
        ]


insertPoints : Options -> Track -> ( Track, String )
insertPoints options track =
    -- Introduce additional trackpoints in all segments **between** markers.
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
            "Insert between "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " and "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        newPointsBetween pt1 pt2 =
            let
                trackPointsNeeded =
                    -- Including the final one.
                    -- Replacing this should make it easier for the multiple segment case.
                    ceiling <|
                        (inMeters <| Vector3d.length pt1.roadVector)
                            / options.maxSpacing
            in
            List.map
                (\i ->
                    Point3d.interpolateFrom
                        pt1.xyz
                        pt2.xyz
                        (toFloat i / toFloat trackPointsNeeded)
                )
                (List.range 1 trackPointsNeeded)

        totalTrackPointsBefore =
            List.length track.trackPoints

        pointsToInterpolate =
            track.trackPoints
                |> List.take (endPoint.index + 1)
                |> List.drop startPoint.index

        allNewTrackPoints =
            List.map2
                newPointsBetween
                pointsToInterpolate
                (List.drop 1 pointsToInterpolate)
                |> List.concat
                |> List.map trackPointFromPoint

        precedingTrackPoints =
            List.take (startPoint.index + 1) track.trackPoints

        subsequentTrackPoints =
            List.drop (endPoint.index + 1) track.trackPoints

        newTrackPointList =
            precedingTrackPoints ++ allNewTrackPoints ++ subsequentTrackPoints

        currentNode =
            if track.currentNode.index == endPoint.index then
                List.Extra.getAt
                    (endPoint.index
                        + List.length newTrackPointList
                        - totalTrackPointsBefore
                    )
                    track.trackPoints
                    |> Maybe.withDefault track.currentNode

            else
                track.currentNode

        markedNode =
            if track.markedNode == Just endPoint then
                List.Extra.getAt
                    (endPoint.index
                        + List.length newTrackPointList
                        - totalTrackPointsBefore
                    )
                    track.trackPoints
                    |> Maybe.withDefault endPoint
                    |> Just

            else
                Nothing
    in
    ( { track
        | trackPoints = newTrackPointList
        , currentNode = currentNode
        , markedNode = markedNode
      }
    , undoMessage
    )
