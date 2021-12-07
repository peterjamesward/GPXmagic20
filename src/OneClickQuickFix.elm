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
        simplifyTrack =
            --TODO: Beware infinite loop!!
            let
                isCloselySpaced t =
                    let
                        obs =
                            TrackObservations.deriveProblems t TrackObservations.defaultOptions
                    in
                    obs.meanSpacing < 25.0

                removeSomePoints t =
                    { t | trackPoints = t |> Straightener.simplifyWithDefaults }
            in
            Loop.while (isCloselySpaced) removeSomePoints

        interpolateTrack track =
            { track | trackPoints = track |> Interpolate.interpolateWithDefaults }

        smoothTrack track =
            { track | trackPoints = track |> Filters.smoothWithDefaults }

        bezierApprox track =
            { track | trackPoints = track |> Filters.bezierWithDefaults }

        finalTrack =
            { originalTrack
                | currentNode = List.head originalTrack.trackPoints |> Maybe.withDefault originalTrack.currentNode
                , markedNode = Nothing
            }
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
