module OneClickQuickFix exposing (oneClickQuickFix)

import Loop
import Straightener
import Track exposing (Track)
import TrackObservations exposing (TrackObservations, defaultOptions, deriveProblems)


type alias MinimalModel m =
    -- Expose only what we use.
    { m
        | track : Maybe Track
    }


oneClickQuickFix : MinimalModel m -> MinimalModel m
oneClickQuickFix model =
    let
        simplifiedTrack =
            Maybe.map simplifyTrack model.track

        simplifyTrack track =
            Loop.while
                (\t -> (deriveProblems t defaultOptions).meanSpacing < 20.0)
                (Straightener.simplifyTrack Straightener.defaultOptions >> Tuple.first)
                track
    in
    model
