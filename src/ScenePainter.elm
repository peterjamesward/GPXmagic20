module ScenePainter exposing (..)

import Angle exposing (Angle, inDegrees)
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveZ)
import Element exposing (Attribute, Element, el, fill, html, htmlAttribute, none, pointer, width)
import Html.Attributes exposing (style)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Length, Meters, meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (Scene)
import TrackPoint exposing (TrackPoint)
import Viewpoint3d exposing (Viewpoint3d)


view3dHeight =
    700


view3dWidth =
    1000


type ImageMsg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOpMsg
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event


type PostUpdateAction
    = ImageOnly
    | PointerMove TrackPoint
    | FocusMove TrackPoint


withMouseCapture : (ImageMsg -> msg) -> List (Attribute msg)
withMouseCapture wrap =
    [ htmlAttribute <| Mouse.onDown (ImageGrab >> wrap)
    , htmlAttribute <| Mouse.onMove (ImageDrag >> wrap)
    , htmlAttribute <| Mouse.onUp (ImageRelease >> wrap)
    , htmlAttribute <| Mouse.onClick (ImageClick >> wrap)
    , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> wrap)
    , htmlAttribute <| Wheel.onWheel (\event -> wrap (ImageMouseWheel event.deltaY))
    , htmlAttribute <| style "touch-action" "none"
    , onContextMenu (wrap ImageNoOpMsg)
    , width fill
    , pointer
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


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , distance : Length
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , clickedPoint : Maybe TrackPoint
    , sceneSearcher : Axis3d Meters LocalCoords -> Maybe TrackPoint
    , viewpoint : Viewpoint3d Meters LocalCoords
    , camera : Camera3d Meters LocalCoords
    }


defaultViewingContext : ViewingContext
defaultViewingContext =
    let
        azimuth =
            Angle.degrees -90.0

        elevation =
            Angle.degrees 40.0

        distance =
            Length.meters 100.0

        focalPoint =
            Point3d.origin

        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = focalPoint
                , azimuth = azimuth
                , elevation = elevation
                , distance = distance
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { azimuth = azimuth
    , elevation = elevation
    , distance = distance
    , orbiting = Nothing
    , zoomLevel = 12.0
    , focalPoint = focalPoint
    , clickedPoint = Nothing
    , sceneSearcher = always Nothing
    , viewpoint = viewpoint
    , camera = camera
    }


initialiseView : List TrackPoint -> ViewingContext
initialiseView track =
    -- This is just a simple default so we can see something!
    let
        firstPointOnTrack =
            track
                |> List.head
                |> Maybe.map .xyz
                |> Maybe.withDefault Point3d.origin
    in
    { defaultViewingContext
        | focalPoint = firstPointOnTrack
    }


viewWebGLContext :
    ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewWebGLContext context scene wrapper =
    el
        (withMouseCapture wrapper)
    <|
        html <|
            Scene3d.sunny
                { camera = context.camera
                , dimensions = ( Pixels.pixels view3dWidth, Pixels.pixels view3dHeight )
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = False
                }


update : ImageMsg -> ViewingContext -> ( ViewingContext, PostUpdateAction )
update msg view =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    event.keys.ctrl || event.button == SecondButton
            in
            ( { view | orbiting = Just event.offsetPos }
            , ImageOnly
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case view.orbiting of
                Just ( startX, startY ) ->
                    let
                        newAzimuth =
                            Angle.degrees <|
                                inDegrees view.azimuth
                                    - (dx - startX)

                        newElevation =
                            Angle.degrees <|
                                inDegrees view.elevation
                                    + (dy - startY)

                        viewpoint =
                            Viewpoint3d.orbitZ
                                { focalPoint = view.focalPoint
                                , azimuth = view.azimuth
                                , elevation = view.elevation
                                , distance = view.distance
                                }

                        camera =
                            Camera3d.perspective
                                { viewpoint = viewpoint
                                , verticalFieldOfView = Angle.degrees 30
                                }
                    in
                    ( { view
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , orbiting = Just ( dx, dy )
                        , viewpoint = viewpoint
                        , camera = camera
                      }
                    , ImageOnly
                    )

                _ ->
                    ( view, ImageOnly )

        ImageRelease _ ->
            ( { view | orbiting = Nothing }, ImageOnly )

        ImageMouseWheel _ ->
            ( view, ImageOnly )

        ImageClick event ->
            case detectHit view event of
                Just tp ->
                    ( { view | focalPoint = tp.xyz }
                    , PointerMove tp
                    )

                Nothing ->
                    ( view, ImageOnly )

        ImageDoubleClick event ->
            case detectHit view event of
                Just tp ->
                    ( { view | focalPoint = tp.xyz }
                    , FocusMove tp
                    )

                Nothing ->
                    ( view, ImageOnly )

        ImageNoOpMsg ->
            ( view, ImageOnly )


detectHit : ViewingContext -> Mouse.Event -> Maybe TrackPoint
detectHit context event =
    -- Same but in our local coord, not map coords.
    -- Need assistance from elm-3d-scene.
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        screenRectangle =
            Rectangle2d.with
                { x1 = Pixels.pixels 0
                , y1 = Pixels.pixels view3dHeight
                , x2 = Pixels.pixels view3dWidth
                , y2 = Pixels.pixels 0
                }

        ray =
            Camera3d.ray context.camera screenRectangle screenPoint
    in
    context.sceneSearcher ray
