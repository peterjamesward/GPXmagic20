module ScenePainterProfile exposing (..)

-- This is our PROFILE view screen painter.

import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color
import Delay
import Direction3d exposing (positiveZ)
import DisplayOptions exposing (DisplayOptions)
import EarthConstants exposing (metresPerPixel, metresPerPixelAtEquatorZoomZero)
import Element exposing (..)
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
import TrackPoint exposing (TrackPoint)
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
        profileTrack =
            track.trackPoints

        ( zoom, centralPoint ) =
            profileZoomLevelFromBoundingBox viewSize profileTrack
    in
    { oldContext
        | focalPoint = track.currentNode.profileXZ
        , sceneSearcher = profilePointNearestRay profileTrack
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewProfile
    }


profileZoomLevelFromBoundingBox :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> List TrackPoint
    -> ( Float, Point3d Length.Meters LocalCoords )
profileZoomLevelFromBoundingBox ( viewWidth, viewHeight ) points =
    let
        lastPoint =
            points
                |> List.Extra.last
                |> Maybe.map .profileXZ
                |> Maybe.withDefault Point3d.origin

        firstPoint =
            points
                |> List.head
                |> Maybe.map .profileXZ
                |> Maybe.withDefault Point3d.origin

        midPoint =
            Point3d.midpoint firstPoint lastPoint

        width =
            lastPoint
                |> Point3d.xCoordinate
                |> Length.inMeters

        horizontalMetresPerPixel =
            width / (toFloat <| inPixels viewWidth)

        zoom =
            logBase 2 (metresPerPixelAtEquatorZoomZero / horizontalMetresPerPixel)
    in
    ( clamp 0.0 22.0 zoom, midPoint )


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
            Scene3d.unlit
                { camera = deriveViewPointAndCamera context options
                , dimensions = context.size
                , background = Scene3d.backgroundColor Color.lightCharcoal
                , clipDepth = Length.meters 1
                , entities = scene
                }


deriveViewPointAndCamera : ViewingContext -> DisplayOptions -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view options =
    let
        lookingAtBeforeScaling =
            case view.flythrough of
                Just flying ->
                    let
                        riderXYZ =
                            Point3d.toRecord inMeters flying.cameraPosition
                    in
                    { riderXYZ | x = flying.metresFromRouteStart }

                Nothing ->
                    Point3d.toRecord inMeters view.focalPoint

        scaledFocus =
            { lookingAtBeforeScaling | z = lookingAtBeforeScaling.z * options.verticalExaggeration }
                |> Point3d.fromRecord meters

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 -1000.0 0.0)
                scaledFocus

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = scaledFocus
                , eyePoint = eyePoint
                , upDirection = positiveZ
                }

        camera =
            Camera3d.orthographic
                { viewpoint = viewpoint
                , viewportHeight =
                    -- factor here is empirical.
                    Length.meters <| 500.0 * metresPerPixel view.zoomLevel 0.0
                }
    in
    camera


update :
    ImageMsg
    -> ViewingContext
    -> DisplayOptions
    -> (ImageMsg -> msg)
    -> ( ViewingContext, PostUpdateAction trck (Cmd msg) )
update msg view options wrap =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageGrab event ->
            ( { view
                | orbiting = Just event.offsetPos
                , dragAction = DragProfile
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
                ( DragProfile, Just ( startX, startY ) ) ->
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
                case detectHit view options event of
                    Just tp ->
                        ( view, ActionPointerMove tp )

                    Nothing ->
                        ( view, ActionNoOp )

            else
                ( view, ActionNoOp )

        ImageDoubleClick event ->
            case detectHit view options event of
                Just tp ->
                    ( { view | focalPoint = tp.profileXZ }
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
                | zoomLevel = view.defaultZoomLevel
              }
            , ActionNoOp
            )

        _ ->
            ( view, ActionNoOp )


detectHit : ViewingContext -> DisplayOptions -> Mouse.Event -> Maybe TrackPoint
detectHit context options event =
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
            deriveViewPointAndCamera context options

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    context.sceneSearcher ray


profilePointNearestRay : List TrackPoint -> Axis3d Meters LocalCoords -> Maybe TrackPoint
profilePointNearestRay track ray =
    -- Spatial index does not help this.
    let
        x =
            ray |> Axis3d.originPoint |> Point3d.xCoordinate
    in
    track
        |> List.Extra.find
            (\pt ->
                (pt.distanceFromStart |> Quantity.lessThanOrEqualTo x)
                    && (pt.distanceFromStart |> Quantity.plus pt.length |> Quantity.greaterThan x)
            )


changeFocusTo : Track -> ViewingContext -> ViewingContext
changeFocusTo track context =
    { context
        | focalPoint = track.currentNode.profileXZ
        , currentPoint = Just track.currentNode
    }
