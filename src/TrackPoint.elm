module TrackPoint exposing (..)

import Angle exposing (Angle)
import Area
import Axis3d exposing (Axis3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import EarthConstants exposing (metresPerDegree)
import Element exposing (..)
import Json.Encode as E
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra as Maybe
import Plane3d
import Point3d exposing (Point3d, distanceFromAxis)
import Quantity exposing (Quantity)
import SketchPlane3d
import Triangle3d
import Utils exposing (bearingToDisplayDegrees, showDecimal2)
import Vector3d exposing (Vector3d)


type alias TrackPoint =
    -- See if we can manage with just the one system.
    -- Only use lat, lon for I/O!
    { xyz : Point3d Length.Meters LocalCoords
    , profileXZ : Point3d Length.Meters LocalCoords
    , costMetric : Float
    , index : Int
    , distanceFromStart : Quantity Float Meters
    , beforeDirection : Maybe (Direction3d LocalCoords)
    , afterDirection : Maybe (Direction3d LocalCoords)
    , effectiveDirection : Maybe (Direction3d LocalCoords)
    , directionChange : Maybe Angle
    , gradientChange : Maybe Float
    , roadVector : Vector3d Length.Meters LocalCoords
    }


trackPointFromPoint : Point3d Meters LocalCoords -> TrackPoint
trackPointFromPoint point =
    -- For some compatibility bringing v1.0 stuff over.
    { xyz = point
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
    }


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
    , profileXZ = location -- Fix when we traverse the track.
    , costMetric = 10.0 ^ 10.0
    , index = 0
    , distanceFromStart = meters 0.0
    , beforeDirection = Nothing
    , afterDirection = Nothing
    , effectiveDirection = Nothing
    , directionChange = Nothing
    , gradientChange = Nothing
    , roadVector = Vector3d.zero
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


applyGhanianTransform : List TrackPoint -> ( List TrackPoint, Vector3d Meters LocalCoords )
applyGhanianTransform points =
    case List.head points of
        Nothing ->
            ( [], Vector3d.zero )

        Just base ->
            let
                toOrigin =
                    base.xyz
                        |> Point3d.projectOnto Plane3d.xy
                        |> Vector3d.from Point3d.origin
                        |> Vector3d.reverse
            in
            ( List.map
                (\p -> { p | xyz = Point3d.translateBy toOrigin p.xyz })
                points
            , toOrigin
            )


gradientFromPoint : TrackPoint -> Float
gradientFromPoint pt =
    pt.roadVector
        |> Vector3d.direction
        |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
        |> Maybe.withDefault Quantity.zero
        |> Angle.tan
        |> (*) 100.0


prepareTrackPoints : List TrackPoint -> List TrackPoint
prepareTrackPoints trackPoints =
    -- This is where we "enrich" the track points so they
    -- have an index, start distance, a "bearing" and a "cost metric".
    let
        -- We are blowing the stack and I suspect this is the culprit.
        -- Let's try without the ugly explicit recursion.
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
                (\pt dist -> pt.roadVector |> Vector3d.length |> Quantity.plus dist)
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


trackPointBearing : TrackPoint -> TrackPoint -> Maybe (Direction3d LocalCoords)
trackPointBearing from to =
    let
        fromXY =
            Point3d.projectOnto Plane3d.xy from.xyz

        toXY =
            Point3d.projectOnto Plane3d.xy to.xyz
    in
    Direction3d.from fromXY toXY


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
