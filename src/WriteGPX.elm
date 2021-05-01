module WriteGPX exposing (writeGPX)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Track exposing (Track, removeGhanianTransform)
import TrackPoint exposing (..)


preamble =
    """<?xml version='1.0' encoding='UTF-8'?>
<gpx version="1.1"
  creator="https://www.komoot.de"
  xmlns="http://www.topografix.com/GPX/1/1"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1
  http://www.topografix.com/GPX/1/1/gpx.xsd">
  <metadata>
    <name>Cycling</name>
    <author>
      <link href="https://www.stepwiserefinement.co.uk">
        <text>Fixed by Pete and his GPX viewer</text>
        <type>text/html</type>
      </link>
    </author>
  </metadata>
"""


writePreamble =
    preamble


writeTrackPoint : ( Float, Float, Float ) -> String
writeTrackPoint ( lon, lat, ele ) =
    "<trkpt lat=\""
        ++ String.fromFloat lat
        ++ "\" lon=\""
        ++ String.fromFloat lon
        ++ "\">"
        ++ "<ele>"
        ++ String.fromFloat ele
        ++ "</ele>"
        ++ "</trkpt>\n"


writeTrack : String -> List ( Float, Float, Float ) -> String
writeTrack name trackPoints =
    """
  <trk>
    <name>"""
        ++ name
        ++ """</name>
    <trkseg>
"""
        ++ String.concat (List.map writeTrackPoint trackPoints)
        ++ """    </trkseg>
  </trk>
 """


writeFooter =
    "</gpx>"


writeGPX : Track -> String
writeGPX track =
    let
        points =
            removeGhanianTransform track
                |> List.map pointInEarthCoordinates

        useName =
            case track.trackName of
                Just n ->
                    n

                _ ->
                    "A track from GPXmagic"
    in
    writePreamble
        ++ writeTrack useName points
        ++ writeFooter
