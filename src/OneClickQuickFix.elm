module OneClickQuickFix exposing (oneClickQuickFix)

import Filters
import GradientLimiter
import Interpolate
import Loop
import LoopedTrack
import Quantity
import Straightener
import Track exposing (Track)
import TrackObservations exposing (TrackObservations)
import TrackPoint



{-
   One-click quick-fix.
       - Simplify until mean density >= 25 meters, empirically.
       - Maximum slope 15% up & down.
       - Interpolate to max 10m spacing, say.
       - Centroid x 5
       - Write with same file name (OS will append -1)
       - Button goes in the top bar, not the accordion.
-}


oneClickQuickFix : Track -> Track
oneClickQuickFix originalTrack =
    let
        simplifyTrack =
            let
                isCloselySpaced t =
                    let
                        obs =
                            TrackObservations.deriveProblems t TrackObservations.defaultOptions
                    in
                    obs.meanSpacing < 25.0

                removeSomePoints t =
                    let
                        candidatesForRemoval =
                            Straightener.lookForSimplifications Straightener.defaultOptions t

                        remainingTrackPoints =
                            t
                                |> Straightener.simplifyTrack candidatesForRemoval
                                |> Tuple.first
                                |> .trackPoints
                                |> TrackPoint.prepareTrackPoints
                    in
                    { t | trackPoints = remainingTrackPoints }
            in
            Loop.while
                isCloselySpaced
                removeSomePoints

        interpolateTrack track =
            { track
                | trackPoints =
                    track
                        |> Interpolate.insertPoints Interpolate.defaultOptions
                        |> Tuple.first
                        |> .trackPoints
                        |> TrackPoint.prepareTrackPoints
            }

        smoothTrack track =
            { track
                | trackPoints =
                    track
                        |> Filters.applyWeightedAverageFilter
                            Filters.defaultOptions
                            (LoopedTrack.NotALoop Quantity.zero)
                        |> Filters.temporaryIndices
                        -- ??
                        |> TrackPoint.prepareTrackPoints
            }

        limitGradients track =
            { track
                | trackPoints =
                    track
                        |> GradientLimiter.limitGradient GradientLimiter.defaultOptions
                        |> Tuple.first
                        |> .trackPoints
                        |> TrackPoint.prepareTrackPoints
            }
    in
    -- Ignore markers for Quick Fix.
    { originalTrack
        | currentNode = List.head originalTrack.trackPoints |> Maybe.withDefault originalTrack.currentNode
        , markedNode = Nothing
    }
        |> simplifyTrack
        |> limitGradients
        |> interpolateTrack
        |> Loop.for 5 smoothTrack
