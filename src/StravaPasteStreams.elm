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


pasteStreams : Track -> Int -> Int -> StravaSegmentStreams -> List TrackPoint
pasteStreams track start finish streams =
    if start == finish then
        pointsFromStreams streams track

    else if start < finish then
        pointsFromStreams streams track

    else
        List.reverse <| pointsFromStreams streams track
