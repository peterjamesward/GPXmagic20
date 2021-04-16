module ScenePainterProfile exposing (..)

-- This is our PROFILE view screen painter.

import Angle exposing (Angle, inDegrees)
import Axis3d exposing (Axis3d)
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import EarthConstants exposing (metresPerPixel, metresPerPixelAtEquatorZoomZero)
import Element exposing (..)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (..)
import Time
import TrackPoint exposing (TrackPoint, convertToProfileCoordinates, pointInEarthCoordinates, trackPointNearestRay)
import Vector3d
import ViewPureStyles exposing (defaultRowLayout)
import ViewingContext exposing (ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d exposing (Viewpoint3d)


initialiseView :
    List TrackPoint
    -> ViewingContext
initialiseView track =
    -- This is just a simple default so we can see something!
    let
        profileTrack =
            List.map convertToProfileCoordinates track

        ( zoom, centralPoint ) =
            profileZoomLevelFromBoundingBox profileTrack
    in
    { defaultViewingContext
        | focalPoint = centralPoint
        , sceneSearcher = trackPointNearestRay profileTrack
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewProfile
    }


profileZoomLevelFromBoundingBox : List TrackPoint -> ( Float, Point3d Length.Meters LocalCoords )
profileZoomLevelFromBoundingBox points =
    let
        lastPoint =
            points
                |> List.Extra.last
                |> Maybe.map .xyz
                |> Maybe.withDefault Point3d.origin

        firstPoint =
            points
                |> List.head
                |> Maybe.map .xyz
                |> Maybe.withDefault Point3d.origin

        midPoint =
            Point3d.midpoint firstPoint lastPoint

        width =
            lastPoint
                |> Point3d.xCoordinate
                |> Length.inMeters

        horizontalMetresPerPixel =
            width / view3dWidth

        zoom =
            logBase 2 (metresPerPixelAtEquatorZoomZero / horizontalMetresPerPixel)
    in
    ( clamp 0.0 22.0 zoom, midPoint )


viewScene :
    ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene context scene wrapper =
    row [ spacing 0, padding 0 ]
        [ el
            (withMouseCapture wrapper)
          <|
            html <|
                Scene3d.unlit
                    { camera = deriveViewPointAndCamera context
                    , dimensions = ( Pixels.pixels view3dWidth, Pixels.pixels view3dHeight )
                    , background = Scene3d.backgroundColor Color.lightCharcoal
                    , clipDepth = Length.meters 1
                    , entities = scene
                    }
        , zoomButtons wrapper
        ]


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = view.focalPoint
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
            , ImageOnly
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            ( view
            , ImageOnly
            )

        ImageRelease _ ->
            ( view
            , ImageOnly
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY
            in
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel + increment }
            , ImageOnly
            )

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
                    ( { view | focalPoint = convertToProfileCoordinates tp |> .xyz }
                    , PointerMove tp
                    )

                Nothing ->
                    ( view, ImageOnly )

        ImageNoOpMsg ->
            ( view, ImageOnly )

        ImageZoomIn ->
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel + 0.5 }
            , ImageOnly
            )

        ImageZoomOut ->
            ( { view | zoomLevel = clamp 0.0 22.0 <| view.zoomLevel - 0.5 }
            , ImageOnly
            )

        ImageReset ->
            ( { view
                | azimuth = Angle.degrees -90.0
                , elevation = Angle.degrees 90.0
                , zoomLevel = view.defaultZoomLevel
              }
            , ImageOnly
            )


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
