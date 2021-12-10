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
        | focalPoint = track.currentNode.profileXZ
        , viewingMode = ViewNewProfile
    }


viewScene :
    Bool
    -> ViewingContext
    -> DisplayOptions
    -> Maybe Track
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options mtrack wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size

        useWidth =
            Pixels.inPixels viewWidth

        useHeight =
            Pixels.inPixels viewHeight // 2
    in
    case mtrack of
        Just track ->
            let
                effectiveTrack =
                    downSelect track.currentNode context.zoomLevel track.trackPoints
            in
            column
                ((inFront <| zoomButtons wrapper)
                    :: withMouseCapture wrapper
                )
                [ wrapChart useWidth useHeight <|
                    altitudeChart
                        (toFloat useWidth)
                        (toFloat <| useHeight)
                        effectiveTrack
                , wrapChart useWidth useHeight <|
                    gradientChart
                        (toFloat useWidth)
                        (toFloat <| useHeight)
                        effectiveTrack
                ]

        Nothing ->
            el [ centerX, centerY ] <| text "Ideally, we'd have a track to show here."


downSelect : TrackPoint -> Float -> List TrackPoint -> List TrackPoint
downSelect current zoom points =
    -- Let's start with a crude linear "zoom" effect.
    -- zoom is [0..22] but default is 12.
    -- Position slider (tp.index) goes from 0 to max index. e.g Hillingdon about 0..99
    -- At 'zoom', we could show 1 / (1 + zoom) fraction of track, 'centred' on current.
    -- At any level, we want to limit # points to (say) 1000 by discarding some points.
    let
        actualNumberOfPoints =
            List.length points

        trackFractionToShow =
            actualNumberOfPoints // (1 + round zoom)

        regionToShow =
            points
                |> List.drop (min current.index (actualNumberOfPoints - trackFractionToShow))
                |> List.take trackFractionToShow

        excessRatio =
            List.length regionToShow // 1000 + 1
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
    -> ( ViewingContext, PostUpdateAction trck (Cmd msg) )
update msg view options wrap =
    -- Second return value indicates whether selection needs to change.
    case msg of
        _ ->
            ( view, ActionNoOp )
