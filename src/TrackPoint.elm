module TrackPoint exposing (..)

import Angle
import Area
import Axis3d exposing (Axis3d)
import Direction2d exposing (Direction2d)
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d, distanceFromAxis)
import Quantity exposing (Quantity)
import SketchPlane3d
import Triangle3d


type alias TrackPoint =
    -- See if we can manage with just the one system.
    -- Only use lat, lon for I/O!
    { xyz : Point3d Length.Meters LocalCoords
    , effectiveDirection : Maybe (Direction2d LocalCoords)
    , costMetric : Float
    , index : Int
    , distanceFromStart : Quantity Float Length.Meters
    }


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    }


metresPerDegree =
    -- a degree of longitude at the equator ...
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
    { xyz = location
    , effectiveDirection = Nothing
    , costMetric = 0.0
    , index = 0
    , distanceFromStart = Quantity.zero
    }


prepareTrackPoints : List TrackPoint -> List TrackPoint
prepareTrackPoints trackPoints =
    -- This is where we "enrich" the track points so they
    -- have an index, start distance, a "bearing" and a "cost metric".
    let
        helper reversed nextIdx lastDistance points =
            -- Note the interesting point here is the second one. The first is context.
            case points of
                [ penultimate, last ] ->
                    -- We can wrap things up now
                    let
                        span =
                            Point3d.distanceFrom penultimate.xyz last.xyz

                        lastPoint =
                            { last
                                | index = nextIdx + 1
                                , costMetric = 10 ^ 10
                                , effectiveDirection = trackPointBearing penultimate last
                                , distanceFromStart = Quantity.plus lastDistance span
                            }
                    in
                    helper
                        (lastPoint :: reversed)
                        (nextIdx + 1)
                        lastPoint.distanceFromStart
                        [ lastPoint ]

                previous :: point :: next :: rest ->
                    let
                        span =
                            Point3d.distanceFrom previous.xyz point.xyz

                        updatedPoint =
                            { point
                                | index = nextIdx
                                , effectiveDirection =
                                    Maybe.map2 meanBearing
                                        (trackPointBearing previous point)
                                        (trackPointBearing point next)
                                , costMetric =
                                    Area.inSquareMeters <|
                                        Triangle3d.area <|
                                            Triangle3d.fromVertices
                                                ( previous.xyz, point.xyz, next.xyz )
                                , distanceFromStart = Quantity.plus lastDistance span
                            }
                    in
                    helper
                        (updatedPoint :: reversed)
                        (nextIdx + 1)
                        updatedPoint.distanceFromStart
                        (point :: next :: rest)

                _ ->
                    -- No more work, just flip the accumulation list.
                    List.reverse reversed
    in
    case trackPoints of
        firstPoint :: secondPoint :: morePoints ->
            helper
                [ { firstPoint
                    | index = 0
                    , costMetric = 10 ^ 10 -- i.e. do not remove me!
                    , effectiveDirection = trackPointBearing firstPoint secondPoint
                  }
                ]
                1
                Quantity.zero
                trackPoints

        _ ->
            []


trackPointBearing : TrackPoint -> TrackPoint -> Maybe (Direction2d LocalCoords)
trackPointBearing from to =
    let
        fromXY =
            Point3d.projectInto SketchPlane3d.xy from.xyz

        toXY =
            Point3d.projectInto SketchPlane3d.xy to.xyz
    in
    Direction2d.from fromXY toXY


meanBearing : Direction2d LocalCoords -> Direction2d LocalCoords -> Direction2d LocalCoords
meanBearing direction1 direction2 =
    -- I think we find the angle of turn, and halve it.
    let
        turnAngle =
            Direction2d.angleFrom direction1 direction2

        halfAngle =
            turnAngle |> Angle.inRadians |> (*) 0.5 |> Angle.radians
    in
    Direction2d.rotateBy halfAngle direction1


trackPointNearestRay : List TrackPoint -> Axis3d Meters LocalCoords -> Maybe TrackPoint
trackPointNearestRay track ray =
    let
        distances =
            List.map
                (\tp ->
                    ( tp
                    , tp.xyz
                    )
                )
                track
    in
    track
        |> List.Extra.minimumBy (Length.inMeters << distanceFromAxis ray << .xyz)
