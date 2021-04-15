module ViewingContext exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Length
import LocalCoords exposing (LocalCoords)
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
    , mouseDownTime : Time.Posix
    , viewingMode : ViewingMode
    , contextId : Int -- ( 0 = Plan, 1 = First Person, 2 = Profile, 3 = Third person)
    }


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 90.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , zoomLevel = 12.0
    , defaultZoomLevel = 12.0
    , focalPoint = Point3d.origin
    , clickedPoint = Nothing
    , sceneSearcher = always Nothing
    , mouseDownTime = Time.millisToPosix 0
    , viewingMode = ViewPlan
    , contextId = 0
    }


newViewingContext : ViewingMode -> ViewingContext
newViewingContext mode =
    { defaultViewingContext | viewingMode = mode }
