module OneClickQuickFix exposing (oneClickQuickFix, oneClickQuickFixTrack, undoOneClickQuickFix)

import BezierSplines
import BoundingBox3d
import Filters
import GradientLimiter
import Interpolate
import Length
import Loop
import LoopedTrack
import Point3d
import PostUpdateActions exposing (EditResult)
import Quantity
import Straightener
import Track exposing (Track)
import TrackObservations exposing (TrackObservations)
import TrackPoint exposing (TrackPoint)



{-
   One-click quick-fix.
       - Simplify until mean density >= 25 meters, empirically.
       - Maximum slope 15% up & down.
       - Interpolate to max 10m spacing, say.
       - Centroid x 5
       - Write with same file name (OS will append -1)
       - Button goes in the top bar, not the accordion.
-}


oneClickQuickFixTrack : Track -> Track
oneClickQuickFixTrack track =
    let
        results =
            oneClickQuickFix track
    in
    { track | trackPoints = results.edited }


oneClickQuickFix : Track -> EditResult
oneClickQuickFix originalTrack =
    let
        simplifyTrack : Track -> Track
        simplifyTrack t =
            let
                meanSpacing =
                    TrackObservations.deriveProblems t TrackObservations.defaultOptions
                        |> .meanSpacing

                somePointsRemoved =
                    Straightener.simplifyWithDefaults t

                pointsRemoved =
                    List.length somePointsRemoved - List.length t.trackPoints
            in
            if meanSpacing < 25.0 && pointsRemoved > 0 then
                simplifyTrack { t | trackPoints = somePointsRemoved }

            else
                { t | trackPoints = somePointsRemoved }

        interpolateTrack : Track -> Track
        interpolateTrack track =
            { track | trackPoints = track |> Interpolate.interpolateWithDefaults }

        smoothTrack : Track -> Track
        smoothTrack track =
            { track | trackPoints = track |> Filters.smoothWithDefaults }

        bezierApprox : Track -> Track
        bezierApprox track =
            { track | trackPoints = track |> Filters.bezierWithDefaults }

        finalTrack = originalTrack
                |> simplifyTrack
                |> bezierApprox
                |> Loop.for 3 smoothTrack
    in
    { before = []
    , edited = finalTrack.trackPoints
    , after = []
    , earthReferenceCoordinates = finalTrack.earthReferenceCoordinates
    }


undoOneClickQuickFix : Track -> EditResult
undoOneClickQuickFix track =
    { before = []
    , edited = track.trackPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }
