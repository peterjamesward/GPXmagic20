module ScenePainterProfileCharts exposing (..)

-- This is our SVG charting profile view screen painter.

import Chart as C
import Chart.Attributes as CA
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Html exposing (Html)
import Length exposing (Meters)
import List.Extra
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
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
        | viewingMode = ViewNewProfile
        , zoomLevel = 12.0
        , chartPoints = downSelect track.currentNode 12.0 track.trackPoints
        , focalPoint = track.currentNode.xyz
    }


viewScene :
    Bool
    -> ViewingContext
    -> DisplayOptions
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size

        useWidth =
            Pixels.inPixels viewWidth

        useHeight =
            Pixels.inPixels viewHeight // 2
    in
    column
        [ inFront <| zoomButtons wrapper ]
        [ wrapChart useWidth useHeight <|
            altitudeChart
                (toFloat useWidth)
                (toFloat <| useHeight)
                context.chartPoints
        , wrapChart useWidth useHeight <|
            gradientChart
                (toFloat useWidth)
                (toFloat <| useHeight)
                context.chartPoints
        ]


downSelect : TrackPoint -> Float -> List TrackPoint -> List TrackPoint
downSelect current zoom points =
    -- Zoom is logarithmic. In MapBox, zoom 0 => 78km per pixel at equator.
    -- That's tiny. We probably want zoom 0 to mean we can show (say) 500km of track.
    -- Enough for Mark Beaumont.
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
    in
    regionToShow
        |> List.filter (\pt -> (pt.index |> modBy excessRatio) == 0)


wrapChart useWidth useHeight =
    el
        [ width <| px useWidth
        , height <| px useHeight
        , padding 5
        ]
        << html


altitudeChart : Float -> Float -> List TrackPoint -> Html msg
altitudeChart w h trackPoints =
    let
        minY =
            List.Extra.minimumBy
                (.xyz >> Point3d.zCoordinate >> Length.inMeters)
                trackPoints
                |> Maybe.map (.xyz >> Point3d.zCoordinate >> Length.inMeters)
                |> Maybe.withDefault 0.0
    in
    C.chart
        [ CA.height h
        , CA.width w
        , CA.margin { top = 20, bottom = 20, left = 20, right = 20 }
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
            trackPoints

        --, C.each model.hovering <|
        --    \p item ->
        --        [ C.tooltip item [] [] [] ]
        ]


gradientChart : Float -> Float -> List TrackPoint -> Html msg
gradientChart w h trackPoints =
    C.chart
        [ CA.height h
        , CA.width w
        , CA.margin { top = 20, bottom = 20, left = 20, right = 20 }
        ]
        [ C.xTicks []
        , C.yTicks []
        , C.xLabels []
        , C.yLabels []
        , C.xAxis [ CA.noArrow ]
        , C.yAxis [ CA.noArrow ]
        , C.series (.distanceFromStart >> Length.inMeters)
            [ C.interpolated gradientFromPoint [ CA.stepped ] [] ]
            trackPoints

        --, C.each model.hovering <|
        --    \p item ->
        --        [ C.tooltip item [] [] [] ]
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
    let
        zoom =
            clamp 0.0 22.0 <| view.zoomLevel + 1.0

        _ =
            Debug.log "update" msg
    in
    case msg of
        ImageZoomIn ->
            ( { view
                | zoomLevel = zoom
                , chartPoints = downSelect track.currentNode zoom track.trackPoints
              }
            , ActionPreview
            )

        ImageZoomOut ->
            ( { view
                | zoomLevel = zoom
                , chartPoints = downSelect track.currentNode zoom track.trackPoints
              }
            , ActionPreview
            )

        ImageReset ->
            ( { view
                | zoomLevel = view.defaultZoomLevel
                , chartPoints = downSelect track.currentNode zoom track.trackPoints
              }
            , ActionPreview
            )

        _ ->
            ( view, ActionNoOp )


changeFocusTo : Track -> ViewingContext -> ViewingContext
changeFocusTo track context =
    { context
        | focalPoint = track.currentNode.xyz
        , currentPoint = Just track.currentNode
        , chartPoints = downSelect track.currentNode context.zoomLevel track.trackPoints
    }
