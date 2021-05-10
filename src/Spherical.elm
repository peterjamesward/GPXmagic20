module Spherical exposing (..)

-- Need some coordinate mangling
-- https://www.movable-type.co.uk/scripts/latlong.html

import Angle exposing (Angle)


meanRadius =
    6371000


metresPerDegree =
    78846.81



-- Equirectangular approximation


range : ( Angle, Angle ) -> ( Angle, Angle ) -> Float
range latLon1 latLon2 =
    let
        ( lat1, lon1 ) =
            ( Angle.inRadians <| Tuple.first latLon1, Angle.inRadians <| Tuple.second latLon1 )

        ( lat2, lon2 ) =
            ( Angle.inRadians <| Tuple.first latLon2, Angle.inRadians <| Tuple.second latLon2 )

        x =
            (lon2 - lon1) * cos ((lat1 + lat2) / 2)

        y =
            lat2 - lat1
    in
    meanRadius * sqrt (x * x + y * y)


findBearingToTarget ( lat1, lon1 ) ( lat2, lon2 ) =
    let
        y =
            sin (lon2 - lon1) * cos lat2

        x =
            cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos (lon2 - lon1)
    in
    atan2 y x



-- Find new lat long after travelling d metres on given bearing.


newPosition : ( Float, Float ) -> Float -> Float -> ( Float, Float )
newPosition ( lat1, lon1 ) d θ =
    let
        δ =
            d / meanRadius

        lat2 =
            asin (sin lat1 * cos δ + cos lat1 * sin δ * cos θ)

        lon2 =
            lon1 + atan2 (sin θ * sin δ * cos lat1) (cos δ - sin lat1 * sin lat2)
    in
    ( lat2, lon2 )


approximateElevation : Float -> Float -> Float
approximateElevation r h =
    let
        a =
            (meanRadius + h) / r

        b =
            r / (meanRadius + h)
    in
    a - b |> sqrt |> acos |> (-) (pi / 2)


cartesianTargetPosition : ( Float, Float ) -> Float -> Float -> ( Float, Float )
cartesianTargetPosition ( lon1, lat1 ) r θ =
    --Find lat and long given range and bearing from a known point.
    --Uses range in meters!
    --where lat is latitude, lon is longitude,
    --θ is the bearing (clockwise from north),
    --δ is the angular distance d/R;
    --d being the distance travelled, R the earth’s radius
    let
        δ =
            r / meanRadius

        lat2 =
            asin (sin lat1 * cos δ + cos lat1 * sin δ * cos θ)

        lon2 =
            lon1
                + atan2
                    (sin θ * sin δ * cos lat1)
                    (cos δ - sin lat1 * sin lat2)
    in
    ( lon2, lat2 )
