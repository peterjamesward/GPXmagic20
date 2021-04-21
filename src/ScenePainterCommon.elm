module ScenePainterCommon exposing (..)

import Axis3d exposing (Axis3d)
import BoundingBox3d
import ColourPalette exposing (white)
import EarthConstants exposing (metresPerPixelAtEquatorZoomZero)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html.Attributes exposing (style)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Point3d exposing (Point3d, distanceFromAxis)
import Quantity exposing (Quantity)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Utils exposing (useIcon)
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


withMouseCapture : (ImageMsg -> msg) -> List (Attribute msg)
withMouseCapture wrap =
    [ htmlAttribute <| Mouse.onDown (ImageGrab >> wrap)
    , htmlAttribute <| Mouse.onMove (ImageDrag >> wrap)
    , htmlAttribute <| Mouse.onUp (ImageRelease >> wrap)
    , htmlAttribute <| Mouse.onClick (ImageClick >> wrap)
    , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> wrap)
    , htmlAttribute <| Wheel.onWheel (\event -> wrap (ImageMouseWheel event.deltaY))
    , htmlAttribute <| style "touch-action" "none"
    , onContextMenu (wrap <| ImageNoOpMsg)
    , width fill
    , pointer
    ]


zoomButtons wrap =
    column
        [ alignTop
        , moveDown 5
        , moveLeft 40
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        ]
        [ button []
            { onPress = Just <| wrap ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , button []
            { onPress = Just <| wrap ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , button []
            { onPress = Just <| wrap ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        ]


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

        zoom =
            logBase 2 (cos (degrees medianLatitude) * metresPerPixelAtEquatorZoomZero / desiredMetresPerPixel)
    in
    ( clamp 0.0 22.0 zoom, BoundingBox3d.centerPoint box )


trackPointNearestRay : List TrackPoint -> Axis3d Meters LocalCoords -> Maybe TrackPoint
trackPointNearestRay track ray =
    track
        |> List.Extra.minimumBy
            (Length.inMeters << distanceFromAxis ray << .xyz)


changeFocusTo : TrackPoint -> ViewingContext -> ViewingContext
changeFocusTo tp context =
    { context | focalPoint = tp.xyz }
