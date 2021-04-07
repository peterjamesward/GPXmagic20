module TrackPoint exposing (..)

type GPXCoords
    = GPXCoords


type alias TrackPoint =
    { gpxCoords : Point3d Length.Meters GPXCoords
    , localCoords : Point3d Length.Meters LocalCoords
    , effectiveDirection : Direction2d
    , costMetric : Float
    }


type alias AtomicSpan =
    { commences : TrackPoint
    , concludes : TrackPoint
    , segment : LineSegment3d Length.Meters LocalCoords
    , effectiveDirection : Direction2d
    , distanceAtStart : Float
    , distanceAtEnd : Float
    }


type alias CompoundSpan =
    { commences : TrackPoint
    , concludes : TrackPoint
    , segment : LineSegment3d Length.Meters LocalCoords
    , effectiveDirection : Direction2d
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
