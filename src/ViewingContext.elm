module ViewingContext exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Chart.Item as CI
import Flythrough exposing (Flythrough)
import Length
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import TrackPoint exposing (TrackPoint)
import ViewingMode exposing (ViewingMode(..))


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , distance : Quantity Float Length.Meters
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , clickedPoint : Maybe TrackPoint
    , sceneSearcher : Axis3d Length.Meters LocalCoords -> Maybe TrackPoint
    , viewingMode : ViewingMode
    , contextId : Int -- ( 0 = Plan, 1 = First Person, 2 = Profile, 3 = Third person)
    , size : (Quantity Int Pixels, Quantity Int Pixels)
    , flythrough : Maybe Flythrough
    , waitingForClickDelay : Bool
    , mapClickToDrag : Bool
    , currentPoint : Maybe TrackPoint
    , chartPoints : List TrackPoint
    , chartHover : (List (CI.One TrackPoint CI.Dot))
    }


type DragAction
    = DragNone
    | DragRotate
    | DragPan
    | DragProfile
    | DragPlan


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 30.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 12.0
    , defaultZoomLevel = 12.0
    , focalPoint = Point3d.origin
    , clickedPoint = Nothing
    , sceneSearcher = always Nothing
    , viewingMode = ViewPlan
    , contextId = 0
    , size = (pixels 800, pixels 500)
    , flythrough = Nothing
    , waitingForClickDelay = False
    , mapClickToDrag = True
    , currentPoint = Nothing
    , chartPoints = []
    , chartHover = []
    }


newViewingContext : ViewingMode -> ViewingContext
newViewingContext mode =
    { defaultViewingContext | viewingMode = mode }

