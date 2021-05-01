module TrackObservations exposing (..)

import Angle exposing (Angle)
import Element exposing (..)
import Length exposing (inMeters, meters)
import List.Extra
import Loop exposing (Loopiness(..))
import Point3d
import Quantity exposing (zero)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal2)
import Vector3d


type alias TrackObservations =
    { abruptBearingChanges : List TrackPoint
    , abruptGradientChanges : List TrackPoint
    , zeroLengths : List TrackPoint
    , loopiness : Loop.Loopiness
    , bearingThreshold : Angle
    , gradientThreshold : Float
    , highestMetres : Float
    , lowestMetres : Float
    , trackLength : Float
    , climbingDistance : Float
    , descendingDistance : Float
    , totalClimbing : Float
    , totalDescending : Float
    }


defaultObservations =
    { abruptBearingChanges = []
    , abruptGradientChanges = []
    , zeroLengths = []
    , loopiness = NotALoop <| meters 0.0
    , bearingThreshold = Angle.degrees 60.0
    , gradientThreshold = 10.0
    , highestMetres = 0.0
    , lowestMetres = 0.0
    , trackLength = 0.0
    , climbingDistance = 0.0
    , descendingDistance = 0.0
    , totalClimbing = 0.0
    , totalDescending = 0.0
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

        trackLength =
            track.track
                |> List.Extra.last
                |> Maybe.map .distanceFromStart
                |> Maybe.withDefault (Length.meters 0.0)
                |> inMeters

        highest =
            justXYZ
                |> List.map (Point3d.zCoordinate >> inMeters)
                |> List.maximum
                |> Maybe.withDefault 0.0

        lowest =
            justXYZ
                |> List.map (Point3d.zCoordinate >> inMeters)
                |> List.minimum
                |> Maybe.withDefault 0.0

        justXYZ =
            List.map .xyz track.track

        vectors =
            List.map2
                Vector3d.from
                justXYZ
                (List.drop 1 justXYZ)

        upVectors =
            List.filter
                (Vector3d.zComponent >> Quantity.greaterThan zero)
                vectors

        downVectors =
            List.filter
                (Vector3d.zComponent >> Quantity.lessThan zero)
                vectors

        ascent =
            upVectors
                |> List.map (Vector3d.zComponent >> inMeters)
                |> List.sum

        descent =
            downVectors
                |> List.map (Vector3d.zComponent >> inMeters)
                |> List.sum
                |> abs

        climbingDistance =
            upVectors
                |> List.map (Vector3d.length >> inMeters)
                |> List.sum

        descendingDistance =
            downVectors
                |> List.map (Vector3d.length >> inMeters)
                |> List.sum
    in
    { options
        | abruptGradientChanges = suddenGradientChanges
        , abruptBearingChanges = suddenBearingChanges
        , zeroLengths = zeroLengths
        , loopiness = loopy
        , trackLength = trackLength
        , highestMetres = highest
        , lowestMetres = lowest
        , climbingDistance = climbingDistance
        , descendingDistance = descendingDistance
        , totalClimbing = ascent
        , totalDescending = descent
    }


overviewSummary : TrackObservations -> Element msg
overviewSummary obs =
    row [ padding 20, centerX ]
        [ column [ spacing 10 ]
            [ text "Highest point "
            , text "Lowest point "
            , text "Track length "
            , text "Climbing distance "
            , text "Elevation gain "
            , text "Descending distance "
            , text "Elevation loss "
            ]
        , column [ spacing 10 ]
            [ text <| showDecimal2 obs.highestMetres
            , text <| showDecimal2 obs.lowestMetres
            , text <| showDecimal2 obs.trackLength
            , text <| showDecimal2 obs.climbingDistance
            , text <| showDecimal2 obs.totalClimbing
            , text <| showDecimal2 obs.descendingDistance
            , text <| showDecimal2 obs.totalDescending
            ]
        ]
