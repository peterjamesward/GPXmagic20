module ScenePainter exposing (..)

import Angle exposing (Angle, inDegrees)
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color
import ColourPalette exposing (white)
import Direction3d exposing (negativeZ, positiveZ)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html.Attributes exposing (style)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (Scene)
import Time
import TrackPoint exposing (TrackPoint)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout)
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
    | ImageZoomIn
    | ImageZoomOut


type
    PostUpdateAction
    -- This experimental pattern for returning information back to
    -- main about what needs to follow, since we can't know about the
    -- program at large, only our small part.
    = ImageOnly
    | PointerMove TrackPoint
    | NoContext


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


zoomButtons wrap =
    let
        useIcon =
            html << FeatherIcons.toHtml [] << FeatherIcons.withSize 30
    in
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
    , distance : Quantity Float Length.Meters
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , clickedPoint : Maybe TrackPoint
    , sceneSearcher : Axis3d Length.Meters LocalCoords -> Maybe TrackPoint
    , mouseDownTime : Time.Posix
    }


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 40.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , zoomLevel = 12.0
    , focalPoint = Point3d.origin
    , clickedPoint = Nothing
    , sceneSearcher = always Nothing
    , mouseDownTime = Time.millisToPosix 0
    }


initialiseView :
    List TrackPoint
    -> (Axis3d Length.Meters LocalCoords -> Maybe TrackPoint)
    -> ViewingContext
initialiseView track searcher =
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
        , sceneSearcher = searcher
    }


viewWebGLContext :
    ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewWebGLContext context scene wrapper =
    row defaultRowLayout
        [ el
            (withMouseCapture wrapper)
          <|
            html <|
                Scene3d.sunny
                    { camera = deriveViewPointAndCamera context
                    , dimensions = ( Pixels.pixels view3dWidth, Pixels.pixels view3dHeight )
                    , background = backgroundColor Color.lightBlue
                    , clipDepth = Length.meters 1
                    , entities = scene
                    , upDirection = positiveZ
                    , sunlightDirection = negativeZ
                    , shadows = False
                    }
        , zoomButtons wrapper
        ]


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = view.focalPoint
                , azimuth = view.azimuth
                , elevation = view.elevation
                , distance = view.distance
                }
    in
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        }


update : ImageMsg -> ViewingContext -> Time.Posix -> ( ViewingContext, PostUpdateAction )
update msg view now =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    event.keys.ctrl || event.button == SecondButton
            in
            ( { view
                | orbiting = Just event.offsetPos
                , mouseDownTime = now
              }
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
                    in
                    ( { view
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , orbiting = Just ( dx, dy )
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
            if Time.posixToMillis now < Time.posixToMillis view.mouseDownTime + 250 then
                case detectHit view event of
                    Just tp ->
                        ( view, PointerMove tp )

                    Nothing ->
                        ( view, ImageOnly )

            else
                ( view, ImageOnly )

        ImageDoubleClick event ->
            case detectHit view event of
                Just tp ->
                    ( { view | focalPoint = tp.xyz }
                    , PointerMove tp
                    )

                Nothing ->
                    ( view, ImageOnly )

        ImageNoOpMsg ->
            ( view, ImageOnly )

        ImageZoomIn ->
            ( { view | zoomLevel = view.zoomLevel + 0.5 }, ImageOnly )

        ImageZoomOut ->
            ( { view | zoomLevel = view.zoomLevel - 0.5 }, ImageOnly )


detectHit : ViewingContext -> Mouse.Event -> Maybe TrackPoint
detectHit context event =
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

        camera =
            deriveViewPointAndCamera context

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    context.sceneSearcher ray
