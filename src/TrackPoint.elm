module TrackPoint exposing (..)

import Angle exposing (Angle)
import Area
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import EarthConstants exposing (metresPerDegree)
import Length exposing (Length, Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra as Maybe
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Spherical
import Triangle3d
import Vector3d exposing (Vector3d)


type alias TrackPoint =
    -- See if we can manage with just the one system.
    -- Only use lat, lon for I/O!
    { xyz : Point3d Length.Meters LocalCoords
    , latLon : ( Angle, Angle )
    , profileXZ : Point3d Length.Meters LocalCoords
    , costMetric : Float
    , index : Int
    , distanceFromStart : Quantity Float Meters
    , beforeDirection : Maybe (Direction3d LocalCoords)
    , afterDirection : Maybe (Direction3d LocalCoords)
    , effectiveDirection : Maybe (Direction3d LocalCoords)
    , directionChange : Maybe Angle
    , gradientChange : Maybe Float
    , roadVector : Vector3d Meters LocalCoords
    , length : Length
    }


trackPointFromPoint : Point3d Meters LocalCoords -> TrackPoint
trackPointFromPoint point =
    -- For some compatibility bringing v1.0 stuff over.
    { xyz = point
    , latLon = ( Quantity.zero, Quantity.zero )
    , profileXZ = Point3d.origin
    , costMetric = 0.0
    , index = 0
    , distanceFromStart = Quantity.zero
    , beforeDirection = Nothing
    , afterDirection = Nothing
    , effectiveDirection = Nothing
    , directionChange = Nothing
    , gradientChange = Nothing
    , roadVector = Vector3d.zero
    , length = Length.meters 0.0
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
        * (Vector3d.zComponent pt.roadVector |> inMeters)
        / (pt.length |> inMeters)


prepareTrackPoints : List TrackPoint -> List TrackPoint
prepareTrackPoints trackPoints =
    -- This is where we "enrich" the track points so they
    -- have an index, start distance, a "bearing" and a "cost metric".
    let
        firstPassSetsForwardLooking =
            List.map2
                deriveForward
                trackPoints
                (List.drop 1 trackPoints)
                ++ List.drop (List.length trackPoints - 1) trackPoints

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
                , gradientChange = Just <| abs (gradientFromPoint prev - gradientFromPoint point)
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
                firstPassSetsForwardLooking

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

        --egregiousDirectionChangesRemoved =
        --    -- Recurse (should only be needed once)
        --    -- if there are any suspicious backward hops in the track.
        --    if List.Extra.find egregiousDirectionChange processedPoints /= Nothing then
        --        processedPoints
        --            |> List.filter (not << egregiousDirectionChange)
        --            |> prepareTrackPoints
        --
        --    else
        --        processedPoints
        --
        --egregiousDirectionChange : TrackPoint -> Bool
        --egregiousDirectionChange point =
        --    case point.directionChange of
        --        Just change ->
        --            abs (Angle.inRadians change) > pi * 0.9
        --
        --        Nothing ->
        --            False
    in
    withDistances


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
