module GpxParser exposing (..)

import Regex
import TrackPoint exposing (Track, TrackPoint, prepareTrackPoints, trackPointFromGPX)


asRegex t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


parseTrackName xml =
    case Regex.find (asRegex "<name>(.*)<\\/name>") xml of
        [] ->
            Nothing

        x :: _ ->
            case x.submatches of
                [] ->
                    Nothing

                n :: _ ->
                    n


parseTrackPoints : String -> Maybe Track
parseTrackPoints xml =
    let
        trkpts =
            Regex.find (asRegex "<trkpt((.|\\n|\\r)*?)trkpt>") xml |> List.map .match

        latitude trkpt =
            Regex.find (asRegex "lat=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        longitude trkpt =
            Regex.find (asRegex "lon=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        elevation trkpt =
            Regex.find (asRegex "<ele>([\\d\\.-]*)<\\/ele>") trkpt |> matches

        trackPoint : String -> Maybe TrackPoint
        trackPoint trkpt =
            case ( latitude trkpt, longitude trkpt, elevation trkpt ) of
                ( (Just lat) :: _, (Just lon) :: _, (Just ele) :: _ ) ->
                    Just <| trackPointFromGPX lon lat ele

                _ ->
                    Nothing

        matches xs =
            List.map value xs

        value x =
            case x.submatches of
                (Just val) :: _ ->
                    String.toFloat val

                _ ->
                    Nothing

        trackPoints =
            trkpts
                |> List.map trackPoint
                |> List.filterMap identity
    in
    case trackPoints of
        [] ->
            Nothing

        n1 :: _ ->
            Just
                { trackName = parseTrackName xml
                , track = prepareTrackPoints trackPoints
                , currentNode = n1
                }
