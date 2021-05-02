module StravaPasteStreams exposing (..)

import Point3d
import StravaTypes exposing (StravaSegment, StravaSegmentStreams)
import Track exposing (Track, trackPointFromLonLat)
import TrackPoint exposing (..)


pasteStreams : Track -> StravaSegment -> StravaSegmentStreams -> List TrackPoint
pasteStreams track segment streams =
    let
        pStartingTrackPoint =
            -- Our first track point will be replaced with the first stream point
            trackPointFromLonLat
                ( segment.start_longitude, segment.start_latitude )
                track

        pEndingTrackPoint =
            -- Our last track point will be replaced with the last stream point
            trackPointFromLonLat
                ( segment.end_longitude, segment.end_latitude )
                track

        pointsFromStreams =
            List.map2
                (\latLon ele ->
                    Point3d.meters latLon.lng latLon.lat ele
                        |> Point3d.translateBy track.transform
                )
                streams.latLngs.data
                streams.altitude.data

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
                                []

                            else if start < finish then
                                pointsFromStreams

                            else
                                List.reverse pointsFromStreams

                        precedingPoints =
                            List.take (min start finish) track.track

                        remainingPoints =
                            List.drop (max start finish + 1) track.track
                    in
                    precedingPoints
                        ++ List.map trackPointFromPoint orientedSegment
                        ++ remainingPoints

                _ ->
                    track.track
    in
    newRoute
