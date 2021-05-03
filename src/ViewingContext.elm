module ViewingContext exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Flythrough exposing (Flythrough)
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Time
import TrackPoint exposing (TrackPoint)
import ViewingMode exposing (ViewingMode(..))


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , distance : Quantity Float Length.Meters
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , clickedPoint : Maybe TrackPoint
    , sceneSearcher : Axis3d Length.Meters LocalCoords -> Maybe TrackPoint
    --, mouseDownTime : Time.Posix
    , viewingMode : ViewingMode
    , contextId : Int -- ( 0 = Plan, 1 = First Person, 2 = Profile, 3 = Third person)
    , size : (Quantity Int Pixels, Quantity Int Pixels)
    , verticalExaggeration : Float
    , flythrough : Maybe Flythrough
    , waitingForClickDelay : Bool
    }


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 30.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , zoomLevel = 12.0
    , defaultZoomLevel = 12.0
    , focalPoint = Point3d.origin
    , clickedPoint = Nothing
    , sceneSearcher = always Nothing
    --, mouseDownTime = Time.millisToPosix 0
    , viewingMode = ViewPlan
    , contextId = 0
    , size = (pixels 800, pixels 600)
    , verticalExaggeration = 1.0
    , flythrough = Nothing
    , waitingForClickDelay = False
    }


newViewingContext : ViewingMode -> ViewingContext
newViewingContext mode =
    { defaultViewingContext | viewingMode = mode }


setExaggeration : Float -> ViewingContext -> ViewingContext
setExaggeration scale context =
    { context | verticalExaggeration = scale }