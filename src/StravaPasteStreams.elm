module StravaPasteStreams exposing (..)

import StravaTypes exposing (StravaSegment, StravaSegmentStreams)
import Track exposing (Track, searchTrackPointFromLonLat)
import TrackPoint exposing (..)


pointsFromStreams streams track =
    -- We need to apply the base point shift but using the original base point.
    -- We can fudge this by prependng it to the track.
    List.map2
        (\latLon ele -> ( latLon.lng, latLon.lat, ele ))
        streams.latLngs.data
        streams.altitude.data
        |> applyGhanianTransform track.earthReferenceCoordinates


pasteStreams : Track -> StravaSegment -> StravaSegmentStreams -> List TrackPoint
pasteStreams track segment streams =
    let
        pStartingTrackPoint =
            -- Our first track point will be replaced with the first stream point
            searchTrackPointFromLonLat
                ( segment.start_longitude, segment.start_latitude )
                track

        pEndingTrackPoint =
            -- Our last track point will be replaced with the last stream point
            searchTrackPointFromLonLat
                ( segment.end_longitude, segment.end_latitude )
                track

        newRoute =
            case ( pStartingTrackPoint, pEndingTrackPoint ) of
                ( Just startingTrackPoint, Just endingTrackPoint ) ->
                    let
                        start =
                            startingTrackPoint.index

                        finish =
                            endingTrackPoint.index

                        orientedSegment =
                            if start == finish then
                                pointsFromStreams streams track

                            else if start < finish then
                                pointsFromStreams streams track

                            else
                                List.reverse <| pointsFromStreams streams track

                        precedingPoints =
                            List.take (min start finish) track.trackPoints

                        remainingPoints =
                            List.drop (max start finish + 1) track.trackPoints
                    in
                    precedingPoints
                        ++ orientedSegment
                        ++ remainingPoints

                _ ->
                    track.trackPoints
    in
    newRoute
