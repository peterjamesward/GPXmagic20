module TrackObservations exposing (..)

import Angle exposing (Angle)
import Length exposing (meters)
import List.Extra
import Loop exposing (Loopiness(..))
import Point3d
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)


type alias TrackObservations =
    { abruptBearingChanges : List TrackPoint
    , abruptGradientChanges : List TrackPoint
    , zeroLengths : List TrackPoint
    , loopiness : Loop.Loopiness
    , bearingThreshold : Angle
    , gradientThreshold : Float
    }


defaultObservations =
    { abruptBearingChanges = []
    , abruptGradientChanges = []
    , zeroLengths = []
    , loopiness = NotALoop <| meters 0.0
    , bearingThreshold = Angle.degrees 60.0
    , gradientThreshold = 10.0
    }


deriveProblems : Track -> TrackObservations -> TrackObservations
deriveProblems track options =
    let
        suddenGradientChanges =
            List.filter
                (.gradientChange
                    >> Maybe.withDefault 0.0
                    >> (\x -> x > options.gradientThreshold)
                )
                track.track

        suddenBearingChanges =
            List.filter
                (.directionChange
                    >> Maybe.withDefault Quantity.zero
                    >> Quantity.abs
                    >> Quantity.greaterThan options.bearingThreshold
                )
                track.track

        zeroLengths =
            List.map2
                (\pt1 pt2 ->
                    if pt1.distanceFromStart == pt2.distanceFromStart then
                        Just pt1

                    else
                        Nothing
                )
                track.track
                (List.drop 1 track.track)
                |> List.filterMap identity

        ( firstPoint, lastPoint ) =
            ( List.head track.track, List.Extra.last track.track )

        loopy =
            case ( firstPoint, lastPoint ) of
                ( Just ptStart, Just ptEnd ) ->
                    let
                        gap =
                            Point3d.distanceFrom ptStart.xyz ptEnd.xyz

                        heightDiff =
                            Point3d.zCoordinate ptStart.xyz
                                |> Quantity.minus (Point3d.zCoordinate ptEnd.xyz)
                                |> Quantity.abs
                    in
                    if
                        (gap |> Quantity.lessThanOrEqualTo (meters 1.0))
                            && (heightDiff |> Quantity.lessThanOrEqualTo (meters 1.0))
                    then
                        IsALoop

                    else if gap |> Quantity.lessThanOrEqualTo (meters 1000.0) then
                        AlmostLoop gap

                    else
                        NotALoop gap

                _ ->
                    NotALoop <| meters 0.0
    in
    { options
        | abruptGradientChanges = suddenGradientChanges
        , abruptBearingChanges = suddenBearingChanges
        , zeroLengths = zeroLengths
        , loopiness = loopy
    }
