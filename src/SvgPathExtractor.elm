module SvgPathExtractor exposing (..)

import ColourPalette exposing (buttonText)
import CubicSpline3d
import Delay
import Element exposing (Element, alignTop, centerX, column, fill, padding, row, spacing, spacingXY, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select as Select
import FlatColors.FlatUIPalette
import Length exposing (Meters)
import LineSegment3d
import Path.LowLevel exposing (DrawTo(..), Mode(..), MoveTo(..), SubPath)
import Path.LowLevel.Parser as PathParser
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity(..))
import Spherical exposing (metresPerDegree)
import SvgParser exposing (SvgNode(..), parseToNode)
import Task
import Vector3d


type alias Options =
    { svg : Result String SvgNode
    , paths : List (List SubPath)
    , points : List (List (Point3d Meters LocalCoords))
    , svgFilename : String
    }


type LocalCoords
    = LocalCoords


empty : Options
empty =
    { svg = Err "Hello world"
    , paths = []
    , points = []
    , svgFilename = "SVG"
    }


type Msg
    = ReadFile
    | FileSelected File
    | FileLoaded String
    | WritePaths (List (List (Point3d Meters LocalCoords)))


update : Msg -> Options -> (Msg -> msg) -> ( Options, Cmd msg )
update msg model wrap =
    case msg of
        ReadFile ->
            ( model
            , Select.file [ "text/svg" ] (wrap << FileSelected)
            )

        FileSelected file ->
            ( { model | svgFilename = File.name file }
            , Task.perform (wrap << FileLoaded) (File.toString file)
            )

        FileLoaded content ->
            let
                svg =
                    parseToNode content

                paths =
                    case svg of
                        Ok isSvg ->
                            parsePaths isSvg

                        _ ->
                            []

                points =
                    List.map convertToPoints paths
            in
            ( { model
                | svg = svg
                , paths = paths
                , points = points
              }
            , Delay.after 100 <| (wrap << WritePaths) points
            )

        WritePaths paths ->
            case paths of
                path :: rest ->
                    let
                        content =
                            drawPath path
                    in
                    ( model
                    , Cmd.batch
                        [ File.Download.string model.svgFilename "text/xml" content
                        , Delay.after 1000 <| (wrap << WritePaths) rest
                        ]
                    )

                _ ->
                    ( model, Cmd.none )


view : (Msg -> msg) -> Element msg
view wrap =
    Input.button
        [ Background.color FlatColors.FlatUIPalette.belizeHole
        , Font.color <| buttonText
        , Font.size 16
        , padding 4
        , Element.focused
            [ Background.color FlatColors.FlatUIPalette.wisteria ]
        ]
        { onPress = Just (wrap ReadFile)
        , label = text "Extract paths from SVG file"
        }


parsePaths : SvgNode -> List (List SubPath)
parsePaths svg =
    -- We know that Autotrace should give a list of child elements for subpaths.
    case svg of
        SvgElement svgElement ->
            List.map parseChild svgElement.children

        _ ->
            []


parseChild : SvgNode -> List SubPath
parseChild svg =
    -- Autotrace encodes draw command for subpath in second element of attribute list.
    case svg of
        SvgElement svgElement ->
            case svgElement.attributes of
                _ :: ( key, value ) :: _ ->
                    case key of
                        "d" ->
                            parseOnePath value

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


parseOnePath : String -> List SubPath
parseOnePath path =
    case PathParser.parse path of
        Ok a ->
            a

        _ ->
            []


drawPaths : List (List (Point3d Meters LocalCoords)) -> Element Msg
drawPaths paths =
    column [ spacing 10, padding 10 ] <|
        List.map (drawPath >> text) paths


drawPath : List (Point3d Meters LocalCoords) -> String
drawPath path =
    path |> writeGPX


type alias PathState =
    { startPoint : Point3d Meters LocalCoords
    , currentPoint : Point3d Meters LocalCoords
    , outputs : List (Point3d Meters LocalCoords)
    }


convertToPoints : List SubPath -> List (Point3d Meters LocalCoords)
convertToPoints path =
    let
        pathState : PathState
        pathState =
            { startPoint = Point3d.origin
            , currentPoint = Point3d.origin
            , outputs = []
            }
    in
    List.foldl followSubPath pathState path
        |> .outputs
        |> List.reverse


followSubPath : SubPath -> PathState -> PathState
followSubPath sub state =
    let
        subPathState =
            case sub.moveto of
                MoveTo Absolute ( x, y ) ->
                    let
                        newPoint =
                            Point3d.meters x y 0.0
                    in
                    { state
                        | currentPoint = newPoint
                        , outputs = [ newPoint ]
                    }

                MoveTo Relative ( dx, dy ) ->
                    let
                        newPoint =
                            state.currentPoint
                                |> Point3d.translateBy (Vector3d.meters dx dy 0.0)
                    in
                    { state
                        | currentPoint = newPoint
                        , outputs = [ newPoint ]
                    }
    in
    List.foldl drawCommand subPathState sub.drawtos


drawCommand : DrawTo -> PathState -> PathState
drawCommand command state =
    -- Handling only Absolute variants and simple Bezier.
    case command of
        LineTo Absolute [ ( x, y ) ] ->
            let
                newPoint =
                    Point3d.meters x y 0.0
            in
            { state | outputs = newPoint :: state.outputs }

        CurveTo Absolute [ ( ( x1, y1 ), ( x2, y2 ), ( xN, yN ) ) ] ->
            let
                spline =
                    CubicSpline3d.fromControlPoints
                        state.currentPoint
                        (Point3d.meters x1 y1 0.0)
                        (Point3d.meters x2 y2 0.0)
                        (Point3d.meters xN yN 0.0)
            in
            { state
                | currentPoint = CubicSpline3d.endPoint spline
                , outputs =
                    (spline
                        |> CubicSpline3d.approximate (Quantity 1.0)
                        |> Polyline3d.segments
                        |> List.map LineSegment3d.endPoint
                        |> List.reverse
                    )
                        ++ state.outputs
            }

        _ ->
            state



{- i know this should be combined with the main WriteGPX but I lack the will. -}


preamble =
    """<?xml version='1.0' encoding='UTF-8'?>
<gpx version="1.1"
  creator="https://www.stepwiserefinement.co.uk"
  xmlns="http://www.topografix.com/GPX/1/1"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1
  http://www.topografix.com/GPX/1/1/gpx.xsd">
  <metadata>
    <name>Cycling</name>
    <author>
      <link href="https://www.stepwiserefinement.co.uk">
        <text>GPXmagic 2.0</text>
        <type>text/html</type>
      </link>
    </author>
  </metadata>
"""


writePreamble =
    preamble


writeTrackPoint : Point3d Meters LocalCoords -> String
writeTrackPoint p =
    let
        ( lat, lon, ele ) =
            Point3d.toTuple Length.inMeters p
    in
    -- The +1.0 here is to avoid scientific notation.
    "<trkpt lat=\""
        ++ String.fromFloat (lat / metresPerDegree + 1.0)
        ++ "\" lon=\""
        ++ String.fromFloat (lon / metresPerDegree + 1.0)
        ++ "\">"
        ++ "<ele>"
        ++ String.fromFloat ele
        ++ "</ele>"
        ++ "</trkpt>\n"


writeTrack : String -> List (Point3d Meters LocalCoords) -> String
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


writeGPX : List (Point3d Meters LocalCoords) -> String
writeGPX points =
    writePreamble
        ++ writeTrack "Track from SVG" points
        ++ writeFooter
