module Filters exposing (..)

import CubicSpline3d exposing (CubicSpline3d)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length
import LineSegment3d exposing (LineSegment3d)
import LocalCoords exposing (LocalCoords)
import Loop exposing (Loopiness(..))
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import PostUpdateActions
import TabCommonElements exposing (wholeTrackTextHelper)
import Track exposing (Track)
import TrackObservations exposing (TrackObservations)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Triangle3d exposing (Triangle3d)
import Utils exposing (showDecimal2)
import Vector3d
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
    -> ( Options, PostUpdateActions.PostUpdateAction )
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
                        | track =
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
                        | track =
                            bezierSplines
                                (observations.loopiness == IsALoop)
                                settings.bezierTension
                                settings.bezierTolerance
                                track.track
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


type alias ControlPoint =
    Point3d Length.Meters LocalCoords


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
            track.track

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

        ( start, finish ) =
            ( startPoint.index, endPoint.index )

        firstPoint =
            -- This is used for wrap-around on loop, in which case use second point
            if loopiness == IsALoop then
                List.take 1 <| List.drop 1 points

            else
                List.take 1 points

        lastPoint =
            -- This is used for wrap-around on loop, in which case use penultimate point
            if loopiness == IsALoop then
                List.take 1 <| List.drop 1 <| List.reverse points

            else
                List.take 1 <| List.reverse points

        filteredLoop =
            List.map3
                (weightedAverage (filterBias / 100.0))
                (lastPoint ++ points)
                points
                (List.drop 1 points ++ firstPoint)

        filtered =
            List.map3
                (weightedAverage (filterBias / 100.0))
                (firstPoint ++ points)
                points
                (List.drop 1 points ++ lastPoint)

        withinRange =
            List.take finish >> List.drop start

        ( fixedFirst, fixedLast ) =
            ( List.take (start + 1) points, List.drop finish points )
    in
    if start == finish && loopiness == IsALoop then
        filteredLoop

    else
        fixedFirst ++ withinRange filtered ++ fixedLast


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


bezierSplines : Bool -> Float -> Float -> List TrackPoint -> List TrackPoint
bezierSplines isLoop tension tolerance trackPoints =
    let
        points =
            -- Shim for v1 code.
            List.map .xyz trackPoints

        firstPoint =
            -- This is used for wrap-around on loop, in which case use second point
            if isLoop then
                List.take 1 <| List.drop 1 points

            else
                List.take 1 points

        lastPoint =
            -- This is used for wrap-around on loop, in which case use penultimate point
            if isLoop then
                List.take 1 <| List.drop 1 <| List.reverse points

            else
                List.take 1 <| List.reverse points

        makeTriangles : List (Triangle3d Length.Meters LocalCoords)
        makeTriangles =
            let
                shiftedBack =
                    if isLoop then
                        lastPoint ++ points

                    else
                        firstPoint ++ points

                shiftedForwards =
                    if isLoop then
                        List.drop 1 points ++ firstPoint

                    else
                        List.drop 1 points ++ lastPoint
            in
            List.map3
                Triangle3d.from
                shiftedBack
                points
                shiftedForwards

        controlPointsFromTriangle :
            Triangle3d Length.Meters LocalCoords
            -> ( ControlPoint, ControlPoint, ControlPoint )
        controlPointsFromTriangle triangle =
            let
                ( _, b, _ ) =
                    Triangle3d.vertices triangle

                ( entryEdge, oppositeEdge, exitEdge ) =
                    Triangle3d.edges triangle

                ( ab, ac, bc ) =
                    ( Length.inMeters <| LineSegment3d.length entryEdge
                    , Length.inMeters <| LineSegment3d.length oppositeEdge
                    , Length.inMeters <| LineSegment3d.length exitEdge
                    )

                ( entryFactor, exitFactor ) =
                    ( -1.0 * tension * ab / (ab + bc)
                    , tension * bc / (ab + bc)
                    )

                controlPointVector =
                    Vector3d.from
                        (LineSegment3d.startPoint oppositeEdge)
                        (LineSegment3d.endPoint oppositeEdge)

                ( entryScaleVector, exitScalevector ) =
                    ( Vector3d.scaleBy entryFactor controlPointVector
                    , Vector3d.scaleBy exitFactor controlPointVector
                    )

                ( entryPoint, exitPoint ) =
                    ( Point3d.translateBy entryScaleVector b
                    , Point3d.translateBy exitScalevector b
                    )
            in
            ( entryPoint, b, exitPoint )

        makeControlPoints : List ( ControlPoint, ControlPoint, ControlPoint )
        makeControlPoints =
            List.map
                controlPointsFromTriangle
                makeTriangles

        makeSpline :
            ( ControlPoint, ControlPoint, ControlPoint )
            -> ( ControlPoint, ControlPoint, ControlPoint )
            -> CubicSpline3d Length.Meters LocalCoords
        makeSpline ( _, start, control1 ) ( control2, end, _ ) =
            CubicSpline3d.fromControlPoints
                start
                control1
                control2
                end

        makeSplines : List (CubicSpline3d Length.Meters LocalCoords)
        makeSplines =
            List.map2
                makeSpline
                makeControlPoints
                (List.drop 1 makeControlPoints)

        asPolylines : List (Polyline3d Length.Meters LocalCoords)
        asPolylines =
            List.map
                (CubicSpline3d.approximate (Length.meters tolerance))
                makeSplines

        asSegments : List (LineSegment3d Length.Meters LocalCoords)
        asSegments =
            List.concatMap
                Polyline3d.segments
                asPolylines

        asPointsAgain : List ControlPoint
        asPointsAgain =
            List.map
                LineSegment3d.startPoint
                (List.take 1 asSegments)
                ++ List.map
                    LineSegment3d.endPoint
                    asSegments
    in
    List.map trackPointFromPoint asPointsAgain
