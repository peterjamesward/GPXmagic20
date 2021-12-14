module ScenePainterCommon exposing (..)

import BoundingBox3d
import Chart.Item as CI
import ColourPalette exposing (white)
import EarthConstants exposing (metresPerPixelAtEquatorZoomZero)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters, inMeters)
import LocalCoords exposing (LocalCoords)
import Mapbox.Element exposing (EventData)
import Pixels exposing (Pixels, inPixels)
import Point3d exposing (Point3d, distanceFromAxis)
import Quantity exposing (Quantity)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Utils exposing (elmuiColour, showDecimal0, showDecimal1, useIcon)
import ViewingContext exposing (ViewingContext)


type ImageMsg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOpMsg
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ImageToggleClickToDragOnMap
    | OnHover (List (CI.One TrackPoint CI.Dot))
    | MapMouseDown EventData
    | MapMouseMove EventData
    | MapMouseUp EventData
    | MapClick EventData
    | MapDoubleClick EventData


withMouseCapture : (ImageMsg -> msg) -> List (Attribute msg)
withMouseCapture wrap =
    [ htmlAttribute <| Mouse.onDown (ImageGrab >> wrap)
    , htmlAttribute <| Mouse.onMove (ImageDrag >> wrap)
    , htmlAttribute <| Mouse.onUp (ImageRelease >> wrap)
    , htmlAttribute <| Mouse.onClick (ImageClick >> wrap)
    , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> wrap)
    , htmlAttribute <| Wheel.onWheel (\event -> wrap (ImageMouseWheel event.deltaY))
    , onContextMenu (wrap ImageNoOpMsg)
    , width fill
    , pointer
    ]


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons wrap =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOpMsg >> wrap)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOpMsg >> wrap)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOpMsg >> wrap)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOpMsg >> wrap)
        ]
        [ button [  ]
            { onPress = Just <| wrap ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , button [  ]
            { onPress = Just <| wrap ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , button [  ]
            { onPress = Just <| wrap ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        ]


headUpDisplay gradient =
    el
        [ alignTop
        , alignLeft
        , moveDown 10
        , moveRight 10
        , Background.color <| elmuiColour <| Utils.gradientColourPastel gradient
        , Font.size 30
        , Font.color white
        , padding 6
        , width <| px 100
        , height <| px 100
        , Border.rounded 100
        , Border.width 2
        , Border.color white
        ]
    <|
        el [ centerX, centerY ] <|
            text (showDecimal1 gradient)


onContextMenu : a -> Element.Attribute a
onContextMenu msg =
    HE.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


zoomLevelFromBoundingBox :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> List TrackPoint
    -> ( Float, Point3d Length.Meters LocalCoords )
zoomLevelFromBoundingBox ( view3dWidth, view3dHeight ) points =
    let
        box =
            BoundingBox3d.hullOfN .xyz points
                |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

        ( width, height, _ ) =
            BoundingBox3d.dimensions box

        ( _, medianLatitude, _ ) =
            pointInEarthCoordinates <| BoundingBox3d.centerPoint box

        horizontalMetresPerPixel =
            inMeters width / (toFloat <| inPixels view3dWidth)

        verticalMetresPerPixel =
            inMeters height / (toFloat <| inPixels view3dHeight)

        desiredMetresPerPixel =
            1.5 * max horizontalMetresPerPixel verticalMetresPerPixel

        zoom = 1 +
            logBase 2 (cos (degrees medianLatitude) * metresPerPixelAtEquatorZoomZero / desiredMetresPerPixel)
    in
    ( clamp 1.0 21.0 zoom, BoundingBox3d.centerPoint box )


changeFocusTo : Track -> ViewingContext -> ViewingContext
changeFocusTo track context =
    { context
        | focalPoint = track.currentNode.xyz
        , currentPoint = Just track.currentNode
    }
