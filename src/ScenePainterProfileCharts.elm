module ScenePainterProfileCharts exposing (..)

-- This is our SVG charting profile view screen painter.

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Html as H exposing (Html)
import Length exposing (Meters)
import List.Extra
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Utils exposing (DownSelected, showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)
import ViewingContext exposing (DragAction(..), ViewingContext)
import ViewingMode exposing (ViewingMode(..))


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Track
    -> ViewingContext
    -> ViewingContext
initialiseView viewSize track oldContext =
    -- This is just a simple default so we can see something!
    { oldContext
        | viewingMode = ViewProfileCharts
        , zoomLevel = 12.0
        , defaultZoomLevel = 12.0
        , chartPoints = downSelect track.currentNode 12.0 track.trackPoints
        , focalPoint = track.currentNode.xyz
    }


viewScene :
    ViewingContext
    -> DisplayOptions
    -> (ImageMsg -> msg)
    -> Element msg
viewScene context options wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size

        useWidth =
            Pixels.inPixels viewWidth

        useHeight =
            Pixels.inPixels viewHeight // 2
    in
    column
        [ inFront <| zoomButtons wrapper, spacing 10 ]
        [ wrapChart useWidth useHeight <|
            altitudeChart
                (toFloat useWidth)
                (toFloat useHeight)
                context
                wrapper
        , el [ centerX ] <|
            text <|
                "Showing "
                    ++ String.fromInt context.chartPoints.countSelected
                    ++ " from "
                    ++ String.fromInt context.chartPoints.countInView
                    ++ (if context.chartPoints.countSelected < context.chartPoints.countInView then
                            ". Zoom in to see more."

                        else
                            "."
                       )
        , wrapChart useWidth useHeight <|
            gradientChart
                (toFloat useWidth)
                (toFloat useHeight)
                context
                wrapper
        ]


downSelect : TrackPoint -> Float -> List TrackPoint -> DownSelected TrackPoint
downSelect current zoom points =
    -- Zoom is logarithmic. In MapBox, zoom 0 => 78km per pixel at equator.
    -- At zoom 22, we should show maybe 5m.
    -- So that's 22 steps for 10^8 down to 10^0.
    -- Let's keep it simple.
    let
        metersToShow =
            10 ^ (8 - zoom / 3)

        startDistance =
            Length.inMeters current.distanceFromStart

        regionToShow =
            points
                |> List.Extra.dropWhile (\pt -> Length.inMeters pt.distanceFromStart < startDistance - metersToShow)
                |> List.Extra.takeWhile (\pt -> Length.inMeters pt.distanceFromStart < startDistance + metersToShow)

        excessRatio =
            1 + List.length regionToShow // 500

        selectedPoints =
            List.filter (\pt -> (pt.index |> modBy excessRatio) == 0) regionToShow
    in
    { selected = selectedPoints
    , countInView = List.length regionToShow
    , countSelected = List.length selectedPoints
    }


wrapChart useWidth useHeight =
    el
        [ width <| px useWidth
        , height <| px useHeight
        , padding 5
        ]
        << html


altitudeChart :
    Float
    -> Float
    -> { a | chartPoints : DownSelected TrackPoint, chartHover : List (CI.One TrackPoint CI.Dot) }
    -> (ImageMsg -> msg)
    -> Html msg
altitudeChart w h context wrapper =
    let
        minY =
            List.Extra.minimumBy
                (.xyz >> Point3d.zCoordinate >> Length.inMeters)
                context.chartPoints.selected
                |> Maybe.map (.xyz >> Point3d.zCoordinate >> Length.inMeters)
                |> Maybe.withDefault 0.0
    in
    C.chart
        [ CA.height h
        , CA.width w
        , CA.margin { top = 20, bottom = 20, left = 20, right = 20 }
        , CE.onMouseMove (wrapper << OnHover) (CE.getNearest CI.dots)
        , CE.onMouseLeave ((wrapper << OnHover) [])
        ]
        [ C.xTicks []
        , C.yTicks []
        , C.xLabels []
        , C.yLabels []
        , C.xAxis [ CA.noArrow ]
        , C.yAxis
            [ CA.noArrow ]
        , C.series (.distanceFromStart >> Length.inMeters)
            [ C.interpolated (.xyz >> Point3d.zCoordinate >> Length.inMeters) [] [] ]
            context.chartPoints.selected
        , C.each context.chartHover <|
            \p dot ->
                let
                    x =
                        CI.getX dot

                    y =
                        CI.getY dot
                in
                [ C.tooltip dot
                    []
                    []
                    [ H.text "Distance: "
                    , H.text (showDecimal0 x)
                    , H.br [] []
                    , H.text " Altitude: "
                    , H.text (showDecimal2 y)
                    ]
                ]
        ]


gradientChart :
    Float
    -> Float
    -> { a | chartPoints : DownSelected TrackPoint, chartHover : List (CI.One TrackPoint CI.Dot) }
    -> (ImageMsg -> msg)
    -> Html msg
gradientChart w h context wrapper =
    C.chart
        [ CA.height h
        , CA.width w
        , CA.margin { top = 20, bottom = 20, left = 20, right = 20 }
        , CE.onMouseMove (wrapper << OnHover) (CE.getNearest CI.dots)
        , CE.onMouseLeave ((wrapper << OnHover) [])
        ]
        [ C.xTicks []
        , C.yTicks []
        , C.xLabels []
        , C.yLabels []
        , C.xAxis [ CA.noArrow ]
        , C.yAxis [ CA.noArrow ]
        , C.series (.distanceFromStart >> Length.inMeters)
            [ C.interpolated gradientFromPoint [ CA.stepped ] [] ]
            context.chartPoints.selected
        , C.each context.chartHover <|
            \p dot ->
                let
                    x =
                        CI.getX dot

                    y =
                        CI.getY dot
                in
                [ C.tooltip dot
                    []
                    []
                    [ H.text "Distance: "
                    , H.text (showDecimal0 x)
                    , H.br [] []
                    , H.text " Gradient: "
                    , H.text (showDecimal2 y)
                    ]
                ]
        ]


update :
    ImageMsg
    -> ViewingContext
    -> DisplayOptions
    -> (ImageMsg -> msg)
    -> Track
    -> ( ViewingContext, PostUpdateAction trck (Cmd msg) )
update msg view options wrap track =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageZoomIn ->
            ( let
                zoom =
                    clamp 0.0 22.0 <| view.zoomLevel + 1.0
              in
              { view
                | zoomLevel = zoom
                , chartPoints = downSelect track.currentNode zoom track.trackPoints
              }
            , ActionPreview
            )

        ImageZoomOut ->
            ( let
                zoom =
                    clamp 0.0 22.0 <| view.zoomLevel - 1.0
              in
              { view
                | zoomLevel = zoom
                , chartPoints = downSelect track.currentNode zoom track.trackPoints
              }
            , ActionPreview
            )

        ImageReset ->
            ( { view
                | zoomLevel = view.defaultZoomLevel
                , chartPoints = downSelect track.currentNode view.defaultZoomLevel track.trackPoints
              }
            , ActionPreview
            )

        OnHover datum ->
            ( { view | chartHover = datum }
            , ActionPreview
            )

        _ ->
            ( view, ActionNoOp )


changeFocusTo : Track -> ViewingContext -> ViewingContext
changeFocusTo track context =
    { context
        | focalPoint = track.currentNode.profileXZ
        , currentPoint = Just track.currentNode
        , chartPoints = downSelect track.currentNode context.zoomLevel track.trackPoints
    }
