module OneClickQuickFix exposing (applyMapElevations, oneClickQuickFix)

import BezierSplines
import BoundingBox3d
import Filters
import GradientLimiter
import Interpolate
import Length
import Loop
import LoopedTrack
import Point3d
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
                        |> TrackPoint.temporaryIndices
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

        bezierApprox track =
            Filters.bezierSplineHelper
                BezierSplines.bezierApproximation
                track
                Filters.defaultOptions.bezierTension
                Filters.defaultOptions.bezierTolerance
                (LoopedTrack.NotALoop Quantity.zero)
    in
    -- Ignore markers for Quick Fix.
    { originalTrack
        | currentNode = List.head originalTrack.trackPoints |> Maybe.withDefault originalTrack.currentNode
        , markedNode = Nothing
    }
        |> simplifyTrack
        |> bezierApprox
        |> Loop.for 3 smoothTrack


applyMapElevations : List Float -> Track -> Track
applyMapElevations elevations track =
    let
        useNewElevation tp ele =
            Point3d.xyz
                (Point3d.xCoordinate tp.xyz)
                (Point3d.yCoordinate tp.xyz)
                (Length.meters ele)
                |> TrackPoint.trackPointFromPoint

        newPoints =
            List.map2
                useNewElevation
                track.trackPoints
                elevations
                |> TrackPoint.prepareTrackPoints
    in
    { track
        | trackPoints = newPoints
        , box =
            BoundingBox3d.hullOfN .xyz newPoints
                |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
    }
