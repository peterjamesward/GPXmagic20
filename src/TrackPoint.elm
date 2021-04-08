module TrackPoint exposing (..)

import Direction2d exposing (Direction2d)
import Length
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)



type alias TrackPoint =
    { localCoords : Point3d Length.Meters LocalCoords

    -- Is dual coordinate system a source of problems?
    -- Shall we try with just the one?
    --, gpxCoords : Point3d Length.Meters GPXCoords
    , effectiveDirection : Direction2d LocalCoords
    , costMetric : Float
    , index : Int
    }


type alias Track =
    List TrackPoint


metresPerDegree =
    78846.81


trackPointFromGPX : Float -> Float -> Float -> TrackPoint
trackPointFromGPX lon lat ele =
    let
        location =
            Point3d.fromTuple
                Length.meters
                ( metresPerDegree * lon * cos (degrees lat)
                , metresPerDegree * lat
                , ele
                )
    in
    { localCoords = location
    , effectiveDirection = Direction2d.degrees 0
    , costMetric = 0.0
    , index = 0
    }
