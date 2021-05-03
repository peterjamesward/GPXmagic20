module ScenePainterFirst exposing (..)

import Angle exposing (inDegrees)
import Camera3d exposing (Camera3d)
import Color
import Delay
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (DisplayOptions)
import EarthConstants exposing (metresPerPixel)
import Element exposing (Element, el, html, none, row)
import Flythrough exposing (eyeHeight)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene exposing (Scene)
import Scene3d exposing (backgroundColor)
import ScenePainterCommon exposing (ImageMsg(..), trackPointNearestRay, withMouseCapture, zoomButtons, zoomLevelFromBoundingBox)
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Vector3d
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Track
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        firstPointOnTrack =
            track.track
                |> List.head
                |> Maybe.map .xyz
                |> Maybe.withDefault centralPoint
                |> Point3d.translateBy (Vector3d.meters 0 0 1)

        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track.track

        viewContext =
            newViewingContext ViewFirstPerson

        _ =
            Debug.log "Azimuth" trackAzimuth

        trackAzimuth =
            track.track
                |> List.head
                |> Maybe.andThen .afterDirection
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.reverse
                |> Direction3d.azimuthIn SketchPlane3d.xy
    in
    { viewContext
        | focalPoint = firstPointOnTrack
        , azimuth = trackAzimuth
        , elevation = Angle.degrees 10
        , sceneSearcher = trackPointNearestRay track.track
        , zoomLevel = 14.0
        , defaultZoomLevel = 14.0
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
                    , ActionNoOp
                    )

                _ ->
                    ( view, ActionNoOp )

        ImageRelease _ ->
            ( { view | orbiting = Nothing }, ActionNoOp )

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
            ( { view | orbiting = Nothing }, ActionNoOp )

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


viewScene :
    Bool
    -> ViewingContext
    -> DisplayOptions
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options scene wrapper =
    row []
        [ if visible then
            el (withMouseCapture wrapper) <|
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

          else
            none
        , zoomButtons wrapper
        ]


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        ( _, latitude, _ ) =
            pointInEarthCoordinates view.focalPoint

        cameraViewpoint =
            case view.flythrough of
                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = flying.cameraPosition
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }

                Nothing ->
                    Viewpoint3d.orbitZ
                        { focalPoint =
                            view.focalPoint
                                |> Point3d.translateBy (Vector3d.meters 0 0 1)
                        , azimuth = view.azimuth
                        , elevation = view.elevation
                        , distance =
                            Length.meters <|
                                50.0
                                    * metresPerPixel view.zoomLevel (degrees latitude)
                        }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees <| 120.0 - view.zoomLevel * 4.0
        }


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
