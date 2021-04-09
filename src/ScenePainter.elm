module ScenePainter exposing (..)

import Angle exposing (Angle, inDegrees)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveZ)
import Element exposing (Attribute, Element, el, fill, html, htmlAttribute, none, pointer, width)
import Html.Attributes exposing (style)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Length, meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (Scene)
import TrackPoint exposing (TrackPoint)
import Viewpoint3d exposing (Viewpoint3d)


type
    ImageMsg
    -- Messages that change only the WebGL context, not the model
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOpMsg


type ModelUpdatingMsg
    = ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event


withMouseCapture : (ImageMsg -> msg) -> List (Attribute msg)
withMouseCapture wrap =
    [ htmlAttribute <| Mouse.onDown (\event -> wrap (ImageGrab event))
    , htmlAttribute <| Mouse.onMove (\event -> wrap (ImageDrag event))
    , htmlAttribute <| Mouse.onUp (\event -> wrap (ImageRelease event))

    -- These two are model updating messages, not just view changes.
    --, htmlAttribute <| Mouse.onClick (\event -> wrap (ImageClick event))
    --, htmlAttribute <| Mouse.onDoubleClick (\event -> wrap (ImageDoubleClick event))
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
    }


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 40.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , zoomLevel = 12.0
    , focalPoint = Point3d.origin
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
    { defaultViewingContext | focalPoint = firstPointOnTrack }


viewWebGLContext :
    ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewWebGLContext context scene wrapper =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = context.focalPoint
                , azimuth = context.azimuth
                , elevation = context.elevation
                , distance = context.distance
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    el
        (withMouseCapture wrapper)
    <|
        html <|
            Scene3d.sunny
                { camera = camera
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = False
                }


update : ImageMsg -> ViewingContext -> ViewingContext
update msg view =
    case msg of
        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    event.keys.ctrl || event.button == SecondButton
            in
            { view | orbiting = Just event.offsetPos }

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
                    in
                    { view
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , orbiting = Just ( dx, dy )
                    }

                _ ->
                    view

        ImageRelease _ ->
            { view | orbiting = Nothing }

        ImageMouseWheel _ ->
            view

        ImageNoOpMsg ->
            view
