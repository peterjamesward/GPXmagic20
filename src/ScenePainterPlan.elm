module ScenePainterPlan exposing (..)

-- This is our PLAN view screen painter.

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import EarthConstants exposing (metresPerPixel)
import Element exposing (..)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import PostUpdateActions exposing (PostUpdateAction(..))
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene exposing (Scene)
import Scene3d exposing (Entity, backgroundColor)
import ScenePainterCommon exposing (..)
import Time
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Vector3d
import ViewingContext exposing (ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d exposing (Viewpoint3d)


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> List TrackPoint
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track
    in
    { defaultViewingContext
        | focalPoint = centralPoint
        , sceneSearcher = trackPointNearestRay track
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

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = view.focalPoint
                , eyePoint = eyePoint
                , upDirection = positiveY
                }

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                view.focalPoint
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = Length.meters <| 1200.0 * metresPerPixel view.zoomLevel latitude
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
            ( view
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
            if Time.posixToMillis now < Time.posixToMillis view.mouseDownTime + 250 then
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
