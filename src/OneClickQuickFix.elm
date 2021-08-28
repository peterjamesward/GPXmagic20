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
       - Simplify until mean density >= 20 meters, empirically.
       - NEW: Maximum slope 15% up & down.
       - Interpolate to max 10m spacing, say.
       - Centroid x N, N = 3?
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

                        _ =
                            Debug.log "start length" <| List.length t.trackPoints

                        _ =
                            Debug.log "after length" <| List.length remainingTrackPoints
                    in
                    { t | trackPoints = remainingTrackPoints }
            in
            Loop.while
                isCloselySpaced
                removeSomePoints

        interpolateTrack track =
            let
                _ =
                    Debug.log "interpolate" (List.length track.trackPoints)
            in
            Interpolate.insertPoints Interpolate.defaultOptions track |> Tuple.first

        smoothTrack track =
            let
                _ =
                    Debug.log "smooth" (List.length track.trackPoints)
            in
            { track
                | trackPoints =
                    Filters.applyWeightedAverageFilter
                        Filters.defaultOptions
                        (LoopedTrack.NotALoop Quantity.zero)
                        track
            }

        thriceSmoothTrack : Track -> Track
        thriceSmoothTrack =
            Loop.for 5 smoothTrack

        limitGradients track =
            track
                |> GradientLimiter.limitGradient GradientLimiter.defaultOptions
                |> Tuple.first
    in
    originalTrack
        |> simplifyTrack
        |> limitGradients
        |> interpolateTrack
        |> thriceSmoothTrack
