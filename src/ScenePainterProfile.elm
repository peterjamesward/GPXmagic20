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
import ViewingContext exposing (ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d exposing (Viewpoint3d)


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Track
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        profileTrack =
            track.track

        ( zoom, centralPoint ) =
            profileZoomLevelFromBoundingBox viewSize profileTrack
    in
    { defaultViewingContext
        | focalPoint = centralPoint
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
    Bool
    -> ViewingContext
    -> DisplayOptions
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options scene wrapper =
    row [ spacing 0, padding 0 ]
        [ if visible then
            el
                (withMouseCapture wrapper)
            <|
                html <|
                    Scene3d.unlit
                        { camera = deriveViewPointAndCamera context
                        , dimensions = context.size
                        , background = Scene3d.backgroundColor Color.lightCharcoal
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
        { x, y, z } =
            Point3d.toRecord inMeters view.focalPoint

        scaledFocus =
            Point3d.fromTuple meters ( x, y, z * view.verticalExaggeration )

        viewpoint =
            case view.flythrough of
                Just flying ->
                    let
                        riderXYZ =
                            Point3d.toRecord inMeters flying.cameraPosition

                        inProfile =
                            Point3d.fromRecord meters
                                { riderXYZ | x = flying.metresFromRouteStart }
                    in
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint
                        , focalPoint = inProfile
                        , upDirection = Direction3d.positiveZ
                        }

                Nothing ->
                    Viewpoint3d.lookAt
                        { focalPoint = scaledFocus
                        , eyePoint = eyePoint
                        , upDirection = positiveZ
                        }

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 -1000.0 0.0)
                view.focalPoint

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
            ( { view | waitingForClickDelay = True }
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
            ( view
            , ActionNoOp
            )

        ImageRelease _ ->
            ( view
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


profilePointNearestRay : List TrackPoint -> Axis3d Meters LocalCoords -> Maybe TrackPoint
profilePointNearestRay track ray =
    -- Probably an easier way than this sledgehammer. But.
    track
        |> List.Extra.minimumBy
            (Length.inMeters << distanceFromAxis ray << .profileXZ)


changeFocusTo : TrackPoint -> ViewingContext -> ViewingContext
changeFocusTo tp context =
    { context | focalPoint = tp.profileXZ }
