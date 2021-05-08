module Filters exposing (..)

import BezierSplines exposing (bezierSplines)
import Element exposing (..)
import Element.Input as Input exposing (button)
import List.Extra
import Loop exposing (Loopiness(..))
import Point3d exposing (Point3d)
import PostUpdateActions
import TabCommonElements exposing (wholeTrackTextHelper)
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackObservations exposing (TrackObservations)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Triangle3d exposing (Triangle3d)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Filters

Centroid averaging reduces the amount of wiggle in a track,
vertically and horizontally, by moving each track point towards
the centre of the triangle formed by the neighbouring points.

You may achieve better results if you first increase the density
of trackpoints in the region by using Track Point Insert.

Bezier splines can work quite well where there are relatively
few track points -- it tries to find a flowing line that passses
through each existing point. The two parametes affect the number
of points it will add to smooth the curves and the 'tension' of
the thread it uses. It's best to play, but the defaults are OK.

Curiously, using a spline and then averaging can produce quite
pleasing results."""


type Msg
    = SetFilterBias Float
    | FilterWeightedAverage
    | SetBezierTension Float
    | SetBezierTolerance Float
    | BezierSplines


type alias Options =
    { filterBias : Float
    , bezierTension : Float
    , bezierTolerance : Float
    }


defaultOptions =
    { filterBias = 50.0
    , bezierTension = 0.5
    , bezierTolerance = 5.0
    }


update :
    Msg
    -> Options
    -> TrackObservations
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction msg )
update msg settings observations track =
    case msg of
        SetFilterBias bias ->
            ( { settings | filterBias = bias }
            , PostUpdateActions.ActionNoOp
            )

        SetBezierTension tension ->
            ( { settings | bezierTension = tension }
            , PostUpdateActions.ActionNoOp
            )

        SetBezierTolerance tolerance ->
            ( { settings | bezierTolerance = tolerance }
            , PostUpdateActions.ActionNoOp
            )

        FilterWeightedAverage ->
            let
                newTrack =
                    { track
                        | trackPoints =
                            applyWeightedAverageFilter
                                track
                                settings.filterBias
                                observations.loopiness
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                newTrack
                "Centroid average"
            )

        BezierSplines ->
            let
                newTrack =
                    { track
                        | trackPoints = bezierSplineHelper
                            track
                            settings.bezierTension
                            settings.bezierTolerance
                            observations.loopiness
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                newTrack
                "Bezier splines"
            )


viewFilterControls : Options -> (Msg -> msg) -> Track -> Element msg
viewFilterControls options wrap track =
    let
        centroidFilterControls =
            [ Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetFilterBias
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Smoothing "
                                ++ String.fromInt (round options.filterBias)
                                ++ "%"
                , min = 0.0
                , max = 100.0
                , step = Just 1.0
                , value = options.filterBias
                , thumb = Input.defaultThumb
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap FilterWeightedAverage
                , label = text <| "Centroid averaging"
                }
            ]

        bezierControls =
            [ Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetBezierTension
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Tension "
                                ++ showDecimal2 options.bezierTension
                , min = 0.0
                , max = 1.0
                , step = Just 0.1
                , value = options.bezierTension
                , thumb = Input.defaultThumb
                }
            , Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetBezierTolerance
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Tolerance "
                                ++ showDecimal2 options.bezierTolerance
                , min = 1.0
                , max = 10.0
                , step = Just 0.5
                , value = options.bezierTolerance
                , thumb = Input.defaultThumb
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap BezierSplines
                , label = text <| "Bezier splines"
                }
            ]
    in
    column [ spacing 10, padding 10, centerX, width fill ]
        [ text "Centroid averaging reduces local deviations."
        , text "Splines create new points to smooth between existing points."
        , row
            [ width fill, spacing 20, centerX ]
            [ column [ width <| fillPortion 1, spacing 10, centerX ] centroidFilterControls
            , column [ width <| fillPortion 1, spacing 10, centerX ] bezierControls
            ]
        , wholeTrackTextHelper track
        ]


type alias FilterFunction =
    (TrackPoint -> Float)
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Float


applyWeightedAverageFilter :
    Track
    -> Float
    -> Loopiness
    -> List TrackPoint
applyWeightedAverageFilter track filterBias loopiness =
    let
        points =
            track.trackPoints

        ( startPoint, endPoint ) =
            case track.markedNode of
                Just marker ->
                    if track.currentNode.index <= marker.index then
                        ( Just track.currentNode, track.markedNode )

                    else
                        ( track.markedNode, Just track.currentNode )

                Nothing ->
                    ( List.head points, List.Extra.last points )

        ( start, finish ) =
            ( Maybe.map .index startPoint |> Maybe.withDefault 0
            , Maybe.map .index endPoint |> Maybe.withDefault (List.length points - 1)
            )

        ( firstPoint, lastPoint ) =
            -- This is used for wrap-around on loop, in which case use second point
            -- Yes, this is expensive code. Might improve one day.
            if loopiness == IsALoop && track.markedNode == Nothing then
                ( List.take 1 <| List.drop 1 points
                , List.take 1 <| List.drop 1 <| List.reverse points
                )

            else
                ( List.take 1 points
                , List.take 1 <| List.reverse points
                )

        withinRange =
            List.take finish >> List.drop start

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )
    in
    if track.markedNode == Nothing && loopiness == IsALoop then
        List.map3
            (weightedAverage (filterBias / 100.0))
            (lastPoint ++ points)
            points
            (List.drop 1 points ++ firstPoint)

    else
        let
            filtered =
                List.map3
                    (weightedAverage (filterBias / 100.0))
                    (firstPoint ++ points)
                    points
                    (List.drop 1 points ++ lastPoint)
        in
        fixedFirst
            ++ withinRange filtered
            ++ fixedLast

bezierSplineHelper :
    Track
    -> Float
    -> Float
    -> Loopiness
    -> List TrackPoint
bezierSplineHelper track tension tolerance loopiness =
    let
        points =
            track.trackPoints

        ( startPoint, endPoint ) =
            case track.markedNode of
                Just marker ->
                    if track.currentNode.index <= marker.index then
                        ( Just track.currentNode, track.markedNode )

                    else
                        ( track.markedNode, Just track.currentNode )

                Nothing ->
                    ( List.head points, List.Extra.last points )

        ( start, finish ) =
            ( Maybe.map .index startPoint |> Maybe.withDefault 0
            , Maybe.map .index endPoint |> Maybe.withDefault (List.length points - 1)
            )

        ( firstPoint, lastPoint ) =
            -- This is used for wrap-around on loop, in which case use second point
            -- Yes, this is expensive code. Might improve one day.
            if loopiness == IsALoop && track.markedNode == Nothing then
                ( List.take 1 <| List.drop 1 points
                , List.take 1 <| List.drop 1 <| List.reverse points
                )

            else
                ( List.take 1 points
                , List.take 1 <| List.reverse points
                )

        withinRange =
            List.take finish >> List.drop start

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )

        splinedSection =
            if track.markedNode == Nothing && loopiness == IsALoop then
                bezierSplines
                    (loopiness == IsALoop)
                    tension
                    tolerance
                    ( points)

            else
                bezierSplines
                    (loopiness == IsALoop)
                    tension
                    tolerance
                    (withinRange points)
    in

        fixedFirst
            ++ splinedSection
            ++ fixedLast



weightedAverage :
    Float
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage bias p0 p1 p2 =
    let
        triangle =
            Triangle3d.fromVertices ( p0.xyz, p1.xyz, p2.xyz )

        centroid =
            Triangle3d.centroid triangle

        newP1 =
            Point3d.interpolateFrom p1.xyz centroid bias
    in
    trackPointFromPoint newP1


