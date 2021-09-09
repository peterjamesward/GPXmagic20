module ScenePainterPlan exposing (..)

-- This is our PLAN view screen painter.

import Angle exposing (Angle, inDegrees)
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Delay
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import EarthConstants exposing (metresPerPixel)
import Element exposing (..)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene exposing (Scene)
import Scene3d exposing (Entity, backgroundColor)
import ScenePainterCommon exposing (..)
import SketchPlane3d
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
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
    let
        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track.trackPoints
    in
    { oldContext
        | focalPoint = centralPoint
        , sceneSearcher = trackPointNearestRay track.trackPoints
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewPlan
    }


viewScene :
    Bool
    -> ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context scene wrapper =
    row [ spacing 0, padding 0 ]
        [ if visible then
            el
                (withMouseCapture wrapper)
            <|
                html <|
                    Scene3d.sunny
                        { camera = deriveViewPointAndCamera context
                        , dimensions = context.size
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = scene
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }

          else
            none
        , zoomButtons wrapper
        ]


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        ( _, latitude, _ ) =
            pointInEarthCoordinates view.focalPoint

        lookingAt =
            Maybe.map .cameraPosition view.flythrough
                |> Maybe.withDefault view.focalPoint

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                lookingAt

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = lookingAt
                , eyePoint = eyePoint
                , upDirection =
                    Direction3d.reverse <|
                        Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy
                            view.azimuth
                            Quantity.zero
                }
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = Length.meters <| 1200.0 * metresPerPixel view.zoomLevel latitude
        }


update :
    ImageMsg
    -> ViewingContext
    -> (ImageMsg -> msg)
    -> ( ViewingContext, PostUpdateAction (Cmd msg) )
update msg view wrap =
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
                , dragAction =
                    if alternate then
                        DragRotate

                    else
                        DragPan
                , waitingForClickDelay = True
              }
            , ActionCommand <| Delay.after 250 (wrap ClickDelayExpired)
            )

        ClickDelayExpired ->
            ( { view | waitingForClickDelay = False }
            , ActionNoOp
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( view.dragAction, view.orbiting ) of
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                (startY - dy)
                                (startX - dx)
                                0.0
                                |> Vector3d.rotateAround Axis3d.z view.azimuth
                                |> Vector3d.scaleBy (metresPerPixel view.zoomLevel (degrees 30))
                    in
                    ( { view
                        | focalPoint =
                            view.focalPoint |> Point3d.translateBy shiftVector
                        , orbiting = Just ( dx, dy )
                      }
                    , ActionNoOp
                    )

                ( DragRotate, Just ( startX, startY ) ) ->
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
                    , ActionNoOp
                    )

                _ ->
                    ( view, ActionNoOp )

        ImageRelease _ ->
            ( { view
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , ActionNoOp
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY
            in
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel + increment }
            , ActionNoOp
            )

        ImageClick event ->
            if view.waitingForClickDelay then
                case detectHit view event of
                    Just tp ->
                        ( view, ActionPointerMove tp )

                    Nothing ->
                        ( view, ActionNoOp )

            else
                ( view, ActionNoOp )

        ImageDoubleClick event ->
            case detectHit view event of
                Just tp ->
                    ( { view | focalPoint = tp.xyz }
                    , ActionFocusMove tp
                    )

                Nothing ->
                    ( view, ActionNoOp )

        ImageNoOpMsg ->
            ( view, ActionNoOp )

        ImageZoomIn ->
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel + 0.5 }
            , ActionNoOp
            )

        ImageZoomOut ->
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel - 0.5 }
            , ActionNoOp
            )

        ImageReset ->
            ( { view
                | azimuth = Angle.degrees -90.0
                , elevation = Angle.degrees 90.0
                , zoomLevel = view.defaultZoomLevel
              }
            , ActionNoOp
            )

        _ ->
            ( view, ActionNoOp )


detectHit : ViewingContext -> Mouse.Event -> Maybe TrackPoint
detectHit context event =
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        ( w, h ) =
            context.size

        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveViewPointAndCamera context

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    context.sceneSearcher ray
