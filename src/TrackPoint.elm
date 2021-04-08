module TrackPoint exposing (..)

import Direction2d exposing (Direction2d)
import Length
import Point3d exposing (Point3d)



--type GPXCoords
--    = GPXCoords


type LocalCoords
    = LocalCoords


type alias TrackPoint =
    { localCoords : Point3d Length.Meters LocalCoords

    -- Is dual coordinate system a source of problems?
    -- Shall we try with just the one?
    --, gpxCoords : Point3d Length.Meters GPXCoords
    , effectiveDirection : Direction2d LocalCoords
    , costMetric : Float
    , index : Int
    }



{-
   -- Spans may be premature optimisation ...
   type alias AtomicSpan =
       { commences : TrackPoint
       , concludes : TrackPoint
       , segment : LineSegment3d Length.Meters LocalCoords
       , effectiveDirection : Direction2d LocalCoords
       , distanceAtStart : Float
       , distanceAtEnd : Float
       }


   type alias CompoundSpan =
       { commences : TrackPoint
       , concludes : TrackPoint
       , segment : LineSegment3d Length.Meters LocalCoords
       , effectiveDirection : Direction2d LocalCoords
       , distanceAtStart : Float
       , distanceAtEnd : Float
       , contains : List Span
       , box : BoundingBox3d Length.Meters LocalCoords
       }


   type
       Span
       -- Unsure about this.
       = AtomicSpan
       | CompoundSpan
-}
