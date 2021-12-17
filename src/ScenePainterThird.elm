module ScenePainterThird exposing (..)

-- This is our third person view screen painter.
-- Build others for profile, plan, first person, and factor out common bits.

import Angle exposing (Angle, inDegrees)
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Delay
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (DisplayOptions)
import EarthConstants exposing (metresPerPixel)
import Element exposing (..)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene exposing (Scene)
import Scene3d exposing (Entity, backgroundColor)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import TrackSearchQueries exposing (trackPointNearestFromIndexFor3d)
import Vector3d
import ViewingContext exposing (DragAction(..), ViewingContext, newViewingContext)
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
        | focalPoint = track.currentNode.xyz
        , sceneSearcher = trackPointNearestFromIndexFor3d track.spatialIndex
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
    }


viewScene :
    ViewingContext
    -> DisplayOptions
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene context options scene wrapper =
    el
        ((inFront <| zoomButtons wrapper)
            :: withMouseCapture wrapper
        )
    <|
        html <|
            if options.withLighting then
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
                Scene3d.unlit
                    { camera = deriveViewPointAndCamera context
                    , dimensions = context.size
                    , background = backgroundColor Color.lightBlue
                    , clipDepth = Length.meters 1
                    , entities = scene
                    }


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        ( _, latitude, _ ) =
            pointInEarthCoordinates view.focalPoint

        viewpoint =
            case view.flythrough of
                Just flying ->
                    Viewpoint3d.orbitZ
                        { focalPoint = flying.cameraPosition
                        , azimuth = view.azimuth
                        , elevation = view.elevation
                        , distance =
                            Length.meters <|
                                1000.0
                                    * metresPerPixel view.zoomLevel (degrees latitude)
                        }

                Nothing ->
                    Viewpoint3d.orbitZ
                        { focalPoint = view.focalPoint
                        , azimuth = view.azimuth
                        , elevation = view.elevation
                        , distance =
                            Length.meters <|
                                1000.0
                                    * metresPerPixel view.zoomLevel (degrees latitude)
                        }
    in
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        }


update :
    ImageMsg
    -> ViewingContext
    -> (ImageMsg -> msg)
    -> ( ViewingContext, PostUpdateAction trck (Cmd msg) )
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

                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                ((startY - dy) * Angle.sin view.elevation)
                                (startX - dx)
                                ((dy - startY) * Angle.cos view.elevation)
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
                        ( { view | orbiting = Nothing }, ActionPointerMove tp )

                    Nothing ->
                        ( { view | orbiting = Nothing }, ActionNoOp )

            else
                ( view, ActionNoOp )

        ImageDoubleClick event ->
            case detectHit view event of
                Just tp ->
                    ( { view
                        | focalPoint = tp.xyz
                        , orbiting = Nothing
                      }
                    , ActionFocusMove tp
                    )

                Nothing ->
                    ( { view | orbiting = Nothing }, ActionNoOp )

        ImageNoOpMsg ->
            ( view, ActionNoOp )

        ImageZoomIn ->
            ( { view
                | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel + 0.5
                , orbiting = Nothing
              }
            , ActionNoOp
            )

        ImageZoomOut ->
            ( { view
                | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel - 0.5
                , orbiting = Nothing
              }
            , ActionNoOp
            )

        ImageReset ->
            ( { view
                | azimuth = Angle.degrees -90.0
                , elevation = Angle.degrees 30.0
                , zoomLevel = view.defaultZoomLevel
                , orbiting = Nothing
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
