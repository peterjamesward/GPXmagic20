module TrackPointNew exposing (..)

import Angle exposing (Angle)
import Area
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import EarthConstants exposing (metresPerDegree)
import Length exposing (Length, Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra as Maybe
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d
import SpatialIndex
import Spherical
import Triangle3d
import Vector2d
import Vector3d exposing (Vector3d)



-- Listen up. I'm thinking of reverting to a world where we keep derived data
-- in different structures. Such as below.
-- We recreate TrackPoint and Track functionality bottom-up with clarity and no duplication,
-- them retro-fit into the tools slowly and carefully with nice functional abstractions.
-- So, yeah, its' V3. Let's not pretend otherwise.


type alias RawTrackPoint =
    { latitude : Angle
    , longitude : Angle
    , altitude : Quantity Float Meters
    , index : Int
    }


type alias EuclideanTrackPoint =
    { point : Point3d Meters LocalCoords
    , index : Int
    , source : RawTrackPoint
    , distanceFromStart : Quantity Meters LocalCoords
    }


type alias RoadSection =
    { roadVector : Vector3d Meters LocalCoords
    , startsAt : EuclideanTrackPoint
    , endsAt : EuclideanTrackPoint
    }


type RoadPoint
    = RoadStart RoadSection
    | RoadAnywhere { before : RoadSection, after : RoadSection }
    | RoadEnd RoadSection


type alias Track =
    { rawPoints : List RawTrackPoint
    , xyzPoints : List EuclideanTrackPoint
    , roadSections : List RoadSection
    , roadPoints : List RoadPoint
    , trackName : Maybe String
    , currentNode : RoadPoint
    , markedNode : Maybe RoadPoint
    , graph : Maybe Graph
    , rawBoundingBox : ( Float, Float, Float ) -- (midpoint lon & lat)
    , euclideanBounds : BoundingBox3d Meters LocalCoords
    , spatialIndex : SpatialIndex.SpatialNode RoadPoint Length.Meters LocalCoords
    }



pointInEarthCoordinates : Point3d Length.Meters LocalCoords -> ( Float, Float, Float )
pointInEarthCoordinates point =
    let
        ( x, y, elevation ) =
            Point3d.toTuple Length.inMeters point

        latitude =
            y / metresPerDegree

        longitude =
            x / metresPerDegree / cos (degrees latitude)
    in
    ( longitude, latitude, elevation )


applyGhanianTransform :
    ( Float, Float, Float )
    -> List ( Float, Float, Float )
    -> List TrackPoint
applyGhanianTransform ( baseLon, baseLat, _ ) points =
    let
        toLocalSpace ( lon, lat, ele ) =
            ( metresPerDegree * (lon - baseLon) * cos (degrees baseLat)
            , metresPerDegree * (lat - baseLat)
            , ele
            )
                |> Point3d.fromTuple meters
                |> trackPointFromPoint
                |> (\tp -> { tp | latLon = ( Angle.degrees lat, Angle.degrees lon ) })
    in
    List.map
        toLocalSpace
        points


gradientFromPoint : TrackPoint -> Float
gradientFromPoint pt =
    100.0
        * Quantity.ratio (Vector3d.zComponent pt.roadVector)
            (pt.roadVector |> Vector3d.projectInto SketchPlane3d.xy |> Vector2d.length)


prepareTrackPoints : List TrackPoint -> List TrackPoint
prepareTrackPoints trackPoints =
    -- This is where we "enrich" the track points so they
    -- have an index, start distance, a "bearing" and a "cost metric".
    let
        preFilterDuplicates =
            List.foldl duplicateHelper ( Point3d.origin, [] ) trackPoints
                |> Tuple.second
                |> List.reverse

        duplicateHelper :
            TrackPoint
            -> ( Point3d Meters LocalCoords, List TrackPoint )
            -> ( Point3d Meters LocalCoords, List TrackPoint )
        duplicateHelper point ( lastXYZ, pointsToKeep ) =
            if point.xyz |> Point3d.equalWithin (meters 0.1) lastXYZ then
                ( lastXYZ, pointsToKeep )

            else
                ( point.xyz, point :: pointsToKeep )

        firstPassSetsForwardLooking =
            List.map2
                deriveForward
                preFilterDuplicates
                (List.drop 1 preFilterDuplicates)
                ++ List.drop (List.length preFilterDuplicates - 1) preFilterDuplicates

        deriveForward : TrackPoint -> TrackPoint -> TrackPoint
        deriveForward point nextPt =
            let
                vector =
                    Vector3d.from point.xyz nextPt.xyz
            in
            { point
                | afterDirection =
                    vector
                        |> Vector3d.direction
                        |> Maybe.map (Direction3d.projectOnto Plane3d.xy)
                        |> Maybe.join
                , profileXZ = adjustProfileXZ point.xyz (meters 0.0)
                , roadVector = vector
                , length =
                    meters <|
                        Spherical.range point.latLon nextPt.latLon
            }

        secondPassSetsBackwardLooking =
            List.map3
                deriveBackward
                firstPassSetsForwardLooking
                (List.drop 1 firstPassSetsForwardLooking)
                (List.range 1 (List.length trackPoints))

        deriveBackward : TrackPoint -> TrackPoint -> Int -> TrackPoint
        deriveBackward prev point index =
            { point
                | index = index
                , beforeDirection = prev.afterDirection
                , directionChange = changeInBearing prev.afterDirection point.afterDirection
                , gradientChange =
                    case point.afterDirection of
                        Just weAreNotAtTheEnd ->
                            Just <| abs (gradientFromPoint prev - gradientFromPoint point)

                        Nothing ->
                            Nothing
                , effectiveDirection =
                    Maybe.map2 meanBearing
                        prev.afterDirection
                        point.afterDirection
                , costMetric =
                    Area.inSquareMeters <|
                        Triangle3d.area <|
                            Triangle3d.from
                                prev.xyz
                                point.xyz
                                (point.xyz |> Point3d.translateBy point.roadVector)
            }

        forwardAndBackward =
            List.take 1 firstPassSetsForwardLooking
                ++ secondPassSetsBackwardLooking

        distances =
            List.Extra.scanl
                (\pt dist -> pt.length |> Quantity.plus dist)
                Quantity.zero
                forwardAndBackward

        withDistances =
            List.map2
                (\pt dist ->
                    { pt
                        | distanceFromStart = dist
                        , profileXZ = adjustProfileXZ pt.xyz dist
                    }
                )
                forwardAndBackward
                distances

        egregiousDirectionChangesRemoved =
            -- Recurse (should only be needed once)
            -- if there are any suspicious backward hops in the track.
            if List.Extra.find egregiousDirectionChange withDistances /= Nothing then
                withDistances
                    |> List.filter (not << egregiousDirectionChange)
                    |> prepareTrackPoints

            else
                withDistances

        egregiousDirectionChange : TrackPoint -> Bool
        egregiousDirectionChange point =
            case point.directionChange of
                Just change ->
                    abs (Angle.inRadians change) > pi * 0.9

                Nothing ->
                    False
    in
    withDistances



--egregiousDirectionChangesRemoved


changeInBearing : Maybe (Direction3d LocalCoords) -> Maybe (Direction3d LocalCoords) -> Maybe Angle
changeInBearing before after =
    Maybe.map2 Direction3d.angleFrom before after


meanBearing : Direction3d LocalCoords -> Direction3d LocalCoords -> Direction3d LocalCoords
meanBearing direction1 direction2 =
    -- I think we find the angle of turn, and halve it, rather than average the values.
    -- This is robust for angles > pi/2.
    let
        turnAngle =
            Direction3d.angleFrom direction1 direction2

        halfAngle =
            turnAngle |> Angle.inRadians |> (*) 0.5 |> Angle.radians
    in
    Direction3d.rotateAround Axis3d.z halfAngle direction1


adjustProfileXZ : Point3d Meters LocalCoords -> Quantity Float Meters -> Point3d Meters LocalCoords
adjustProfileXZ point distanceFromStart =
    -- For the profile view, we essentially project onto the XZ plane.
    Point3d.xyz
        distanceFromStart
        Quantity.zero
        (Point3d.zCoordinate point)
