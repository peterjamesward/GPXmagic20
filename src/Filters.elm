module Filters exposing (..)

import BezierSplines exposing (bezierSplines)
import Element exposing (..)
import Element.Input as Input exposing (button)
import List.Extra
import LoopedTrack exposing (Loopiness(..))
import Point3d exposing (Point3d)
import PostUpdateActions
import TabCommonElements exposing (wholeTrackTextHelper)
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackObservations exposing (TrackObservations)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Triangle3d exposing (Triangle3d)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, prettyButtonStyles)


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
    | SetPositionFlag Bool
    | SetElevationFlag Bool
    | BezierApproximation


type alias Options =
    { filterBias : Float
    , bezierTension : Float
    , bezierTolerance : Float
    , applyToPosition : Bool
    , applyToElevation : Bool
    }


defaultOptions : Options
defaultOptions =
    { filterBias = 100.0
    , bezierTension = 0.5
    , bezierTolerance = 5.0
    , applyToPosition = True
    , applyToElevation = True
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

        SetPositionFlag position ->
            ( { settings | applyToPosition = position }
            , PostUpdateActions.ActionNoOp
            )

        SetElevationFlag elevation ->
            ( { settings | applyToElevation = elevation }
            , PostUpdateActions.ActionNoOp
            )

        FilterWeightedAverage ->
            let
                newTrack =
                    { track
                        | trackPoints =
                            temporaryIndices <|
                                applyWeightedAverageFilter
                                    settings
                                    observations.loopiness
                                    track
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
                        | trackPoints =
                            temporaryIndices <|
                                bezierSplineHelper
                                    BezierSplines.bezierSplines
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

        BezierApproximation ->
            let
                newTrack =
                    { track
                        | trackPoints =
                            temporaryIndices <|
                                bezierSplineHelper
                                    BezierSplines.bezierApproximation
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
                "Bezier approximation"
            )


temporaryIndices points =
    List.map2
        (\p i -> { p | index = i })
        points
        (List.range 0 (List.length points))


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
            , Input.checkbox [ moveRight 40 ]
                { onChange = wrap << SetPositionFlag
                , icon = checkboxIcon
                , checked = options.applyToPosition
                , label = Input.labelRight [ centerY ] (text "Position")
                }
            , Input.checkbox [ moveRight 40 ]
                { onChange = wrap << SetElevationFlag
                , icon = checkboxIcon
                , checked = options.applyToElevation
                , label = Input.labelRight [ centerY ] (text "Elevation")
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
                , label = text <| "Bezier splines passing\nthrough existing points"
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap BezierApproximation
                , label = text <| "Bezier approximation\nusing existing points"
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
    Options
    -> Loopiness
    -> Track
    -> List TrackPoint
applyWeightedAverageFilter settings loopiness track =
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
            ( List.take start points, List.drop finish points )
    in
    if track.markedNode == Nothing && loopiness == IsALoop then
        List.map3
            (weightedAverage settings)
            (lastPoint ++ points)
            points
            (List.drop 1 points ++ firstPoint)

    else
        let
            filtered =
                List.map3
                    (weightedAverage settings)
                    (firstPoint ++ points)
                    points
                    (List.drop 1 points ++ lastPoint)
        in
        fixedFirst
            ++ withinRange filtered
            ++ fixedLast


bezierSplineHelper :
    (Bool -> Float -> Float -> List TrackPoint -> List TrackPoint)
    -> Track
    -> Float
    -> Float
    -> Loopiness
    -> List TrackPoint
bezierSplineHelper splineFunction track tension tolerance loopiness =
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
            List.take (finish + 1) >> List.drop start

        ( fixedFirst, fixedLast ) =
            ( List.take start points, List.drop finish points )

        splinedSection =
            case ( loopiness, track.markedNode ) of
                ( IsALoop, Nothing ) ->
                    splineFunction
                        True
                        tension
                        tolerance
                        points

                ( _, _ ) ->
                    splineFunction
                        False
                        tension
                        tolerance
                        (withinRange points)
    in
    fixedFirst
        ++ splinedSection
        ++ fixedLast


weightedAverage :
    Options
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage settings p0 p1 p2 =
    let
        mergePositionWithElevation pXY pZ =
            let
                sourceXY =
                    Point3d.toMeters pXY

                sourceZ =
                    Point3d.toMeters pZ
            in
            { sourceXY | z = sourceZ.z } |> Point3d.fromMeters

        triangle =
            Triangle3d.fromVertices ( p0.xyz, p1.xyz, p2.xyz )

        centroid =
            Triangle3d.centroid triangle

        newP1 =
            Point3d.interpolateFrom p1.xyz centroid (settings.filterBias / 100.0)

        withFlags =
            case ( settings.applyToPosition, settings.applyToElevation ) of
                ( True, True ) ->
                    newP1

                ( False, False ) ->
                    p1.xyz

                ( True, False ) ->
                    -- Use new position, old elevation
                    mergePositionWithElevation newP1 p1.xyz

                ( False, True ) ->
                    mergePositionWithElevation p1.xyz newP1
    in
    trackPointFromPoint withFlags



-- END
