module ScenePainterProfileCharts exposing (..)

-- This is our SVG charting profile view screen painter.

import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Chart as C
import Chart.Attributes as CA
import Color
import Delay
import Direction3d exposing (positiveZ)
import DisplayOptions exposing (DisplayOptions)
import EarthConstants exposing (metresPerPixel, metresPerPixelAtEquatorZoomZero)
import Element exposing (..)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Point2d
import Point3d exposing (Point3d, distanceFromAxis)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene exposing (Scene)
import Scene3d exposing (Entity)
import ScenePainterCommon exposing (..)
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Vector3d
import ViewingContext exposing (DragAction(..), ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d exposing (Viewpoint3d)


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
    -> List TrackPoint
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options track wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size

        useWidth =
            Pixels.inPixels viewWidth

        useHeight =
            Pixels.inPixels viewHeight
    in
    el
        [ width <| px useWidth
        , height <| px useHeight
        , padding 5
        ]
    <|
        html <|
            showCharts
                (toFloat useWidth - 20)
                (toFloat useHeight - 20)
                track


showCharts : Float -> Float -> List TrackPoint -> Html msg
showCharts w h trackPoints =
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

        --, C.bars [] [ C.bar gradientFromPoint [] ] trackPoints
        , C.series (.distanceFromStart >> Length.inMeters)
            [ C.interpolated (.xyz >> Point3d.zCoordinate >> Length.inMeters) [] []
            , C.interpolated gradientFromPoint [ CA.stepped ] []
            ]
            trackPoints
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
