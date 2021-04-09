module ScenePainter exposing (..)

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveZ)
import Element exposing (Element, el, html, none)
import Length
import LocalCoords exposing (LocalCoords)
import Pixels
import Point3d exposing (Point3d)
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (Scene)
import Viewpoint3d


withMouseCapture =
    []



--[ htmlAttribute <| Mouse.onDown (\event -> ImageGrab event)
--, htmlAttribute <| Mouse.onMove (\event -> ImageDrag event)
--, htmlAttribute <| Mouse.onUp (\event -> ImageRelease event)
--, htmlAttribute <| Mouse.onClick (\event -> MouseClick event)
--, htmlAttribute <| Mouse.onDoubleClick (\event -> MouseDoubleClick event)
--, htmlAttribute <| Wheel.onWheel (\event -> MouseWheel event.deltaY)
--, htmlAttribute <| style "touch-action" "none"
--, onContextMenu NoOpMsg
--, width fill
--, pointer
--]


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , zoomLevel : Float
    , camera : Camera3d.Camera3d Length.Meters LocalCoords
    , viewPoint : Point3d Length.Meters LocalCoords
    }


defaultCamera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 500 200 30
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 60
        }


defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 40.0
    , zoomLevel = 12.0
    , camera = defaultCamera
    , viewPoint = Point3d.origin
    }


viewWebGLContext : Camera3d Length.Meters LocalCoords -> Scene -> Element msg
viewWebGLContext camera scene =
    el
        withMouseCapture
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
