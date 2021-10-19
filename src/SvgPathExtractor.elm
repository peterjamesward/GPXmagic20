module SvgPathExtractor exposing (..)

import CubicSpline3d
import Delay
import Element exposing (Element, alignTop, centerX, column, fill, padding, row, spacing, spacingXY, text)
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select as Select
import GpxParser exposing (asRegex)
import Length exposing (Meters)
import LineSegment3d
import Path.LowLevel exposing (DrawTo(..), Mode(..), MoveTo(..), SubPath)
import Path.LowLevel.Parser as PathParser
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity(..))
import Regex
import Spherical exposing (metresPerDegree)
import SvgParser exposing (SvgNode(..))
import Task
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)


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


type alias Path =
    { style : String
    , d : String
    }


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
                --TODO: Must keep paths in separate output files.
                --TODO: Some of the curve following code is close, but wrong.

                pathStrings =
                    Regex.find (asRegex "\\sd=\\\"(.*)\\\"") content
                        |> List.map .submatches
                        |> List.concat
                        |> List.map (Maybe.withDefault "")

                paths =
                    pathStrings |> List.map PathParser.parse

                _ =
                    Debug.log "Paths" paths

                subPaths =
                    paths
                        |> List.filterMap
                            (\p ->
                                case p of
                                    Ok isPath ->
                                        Just isPath

                                    _ ->
                                        Nothing
                            )

                points =
                    subPaths
                        |> List.concat
                        |> convertToPoints
            in
            ( model
            , Delay.after 100 <| (wrap << WritePaths) [points]
            )

        WritePaths paths ->
            case paths of
                path1 :: rest ->
                    let
                        content =
                            drawPath path1
                    in
                    ( model
                    , Cmd.batch
                        [ File.Download.string (model.svgFilename ++ ".gpx") "text/xml" content
                        , Delay.after 1000 <| (wrap << WritePaths) rest
                        ]
                    )

                _ ->
                    ( model, Cmd.none )


view : (Msg -> msg) -> Element msg
view wrap =
    Input.button
        prettyButtonStyles
        { onPress = Just (wrap ReadFile)
        , label = text "Extract paths from SVG file"
        }


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
                        _ =
                            Debug.log "MoveAbsolute" newPoint

                        newPoint =
                            Point3d.meters x y 0.0
                    in
                    { state
                        | currentPoint = newPoint
                        , outputs = [ newPoint ]
                    }

                MoveTo Relative ( dx, dy ) ->
                    let
                        _ =
                            Debug.log "MoveRelative" newPoint

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

        LineTo Relative points ->
            -- This is a fold over the list of points.
            let
                initialState =
                    -- Current point and list of outputs
                    ( state.currentPoint, [] )

                finalState =
                    List.foldl relativeLine initialState points

                relativeLine ( dx, dy ) ( lastPoint, outputs ) =
                    let
                        nextPoint =
                            lastPoint |> Point3d.translateBy (Vector3d.meters dx dy 0.0)
                    in
                    ( nextPoint, nextPoint :: outputs )
            in
            { state
                | currentPoint = Tuple.first finalState
                , outputs = Tuple.second finalState ++ state.outputs
            }

        CurveTo Relative triples ->
            let
                curveRelativeInitialState =
                    ( state.currentPoint, [] )

                curveRelativeFinalState =
                    List.foldl curveRelative curveRelativeInitialState triples

                curveRelative ( ( dx1, dy1 ), ( dx2, dy2 ), ( dxN, dyN ) ) ( lastPoint, outputs ) =
                    let
                        ( c1, c2, cN ) =
                            ( state.currentPoint |> Point3d.translateBy (Vector3d.meters dx1 dy1 0.0)
                            , state.currentPoint |> Point3d.translateBy (Vector3d.meters dx2 dy2 0.0)
                            , state.currentPoint |> Point3d.translateBy (Vector3d.meters dxN dyN 0.0)
                            )

                        spline =
                            -- Should we be using lastPoint or currentPoint??
                            CubicSpline3d.fromControlPoints state.currentPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( CubicSpline3d.endPoint spline, splinePoints ++ outputs)
            in
            { state
                | currentPoint = Tuple.first curveRelativeFinalState
                , outputs =
                    Tuple.second curveRelativeFinalState
                        ++ state.outputs
            }

        CurveTo Absolute triples ->
            let
                curveAbsoluteInitialState =
                    ( state.currentPoint, [] )

                curveAbsoluteFinalState =
                    List.foldl curveAbsolute curveAbsoluteInitialState triples

                curveAbsolute ( ( dx1, dy1 ), ( dx2, dy2 ), ( dxN, dyN ) ) ( lastPoint, outputs ) =
                    let
                        ( c1, c2, cN ) =
                            ( Point3d.meters dx1 dy1 0.0
                            , Point3d.meters dx2 dy2 0.0
                            , Point3d.meters dxN dyN 0.0
                            )

                        spline =
                            -- Should we be using lastPoint or currentPoint??
                            CubicSpline3d.fromControlPoints state.currentPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( CubicSpline3d.endPoint spline, splinePoints ++ outputs)
            in
            { state
                | currentPoint = Tuple.first curveAbsoluteFinalState
                , outputs =
                    Tuple.second curveAbsoluteFinalState
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
