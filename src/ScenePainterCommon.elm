module ScenePainterCommon exposing (..)

import Angle exposing (Angle)
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
import Length exposing (inMeters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SceneBuilder exposing (Scene)
import Time
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Utils exposing (useIcon)


view3dHeight =
    700


view3dWidth =
    1000


type alias ViewingContextId =
    Int


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


type
    PostUpdateAction
    -- This experimental pattern for returning information back to
    -- main about what needs to follow, since we can't know about the
    -- program at large, only our small part.
    = ImageOnly
    | PointerMove TrackPoint
    | ImageNoOp


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
        , moveDown 30
        , moveLeft 80
        , Background.color white
        , Font.size 40
        , padding 10
        , spacing 20
        ]
        [ button
            []
            { onPress = Just <| wrap ImageZoomIn
            , label = useIcon FeatherIcons.zoomIn
            }
        , button
            []
            { onPress = Just <| wrap ImageZoomOut
            , label = useIcon FeatherIcons.zoomOut
            }
        , button
            []
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


type ViewingMode
    = ThirdPerson
    | FirstPerson
    | Profile
    | Plan
    | Map


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , distance : Quantity Float Length.Meters
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , clickedPoint : Maybe TrackPoint
    , sceneSearcher : Axis3d Length.Meters LocalCoords -> Maybe TrackPoint
    , mouseDownTime : Time.Posix
    , viewingMode : ViewingMode
    }


zoomLevelFromBoundingBox : List TrackPoint -> ( Float, Point3d Length.Meters LocalCoords )
zoomLevelFromBoundingBox points =
    let
        box =
            BoundingBox3d.hullOfN .xyz points
                |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

        ( width, height, _ ) =
            BoundingBox3d.dimensions box

        _ =
            Debug.log "size" ( width, height )

        ( _, medianLatitude, _ ) =
            pointInEarthCoordinates <| BoundingBox3d.centerPoint box

        horizontalMetresPerPixel =
            inMeters width / view3dWidth

        verticalMetresPerPixel =
            inMeters height / view3dHeight

        desiredMetresPerPixel =
            1.5 * max horizontalMetresPerPixel verticalMetresPerPixel

        zoom =
            logBase 2 (cos (degrees medianLatitude) * metresPerPixelAtEquatorZoomZero / desiredMetresPerPixel)
    in
    ( clamp 0.0 22.0 zoom, BoundingBox3d.centerPoint box )
