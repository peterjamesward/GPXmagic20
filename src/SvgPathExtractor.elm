module SvgPathExtractor exposing (..)

import AltMath.Matrix4 as AltMath
import BoundingBox3d
import CubicSpline3d
import Element exposing (Element, text)
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Length exposing (Meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Path.LowLevel exposing (DrawTo(..), Mode(..), MoveTo(..), SubPath)
import Path.LowLevel.Parser as PathParser
import Plane3d
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity(..))
import Task
import Track exposing (Track)
import TrackPoint
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)
import XmlParser exposing (Node(..))


type alias Options =
    { track : Maybe Track
    , svgFilename : String
    }


empty : Options
empty =
    { track = Nothing
    , svgFilename = "SVG"
    }


type Msg
    = ReadFile
    | FileSelected File
    | FileLoaded String


type alias Path =
    { style : String
    , d : String
    }


update : Msg -> Options -> (Msg -> msg) -> ( Options, Cmd msg )
update msg model wrap =
    case msg of
        ReadFile ->
            ( { model | track = Nothing }
            , Select.file [ "text/svg" ] (wrap << FileSelected)
            )

        FileSelected file ->
            ( { model | svgFilename = File.name file }
            , Task.perform (wrap << FileLoaded) (File.toString file)
            )

        FileLoaded content ->
            processXML model content


type alias PathInfo =
    { d : Maybe String
    , transform : Maybe String
    }


type alias PathAndTransform =
    { subpaths : List SubPath
    , transform : AltMath.Mat4
    }


identityTransform =
    AltMath.identity


parseTransform : String -> AltMath.Mat4
parseTransform =
    always identityTransform


parsePathInfo : PathInfo -> PathAndTransform
parsePathInfo { d, transform } =
    let
        parsedPath =
            case Maybe.map PathParser.parse d |> Maybe.withDefault (Err []) of
                Ok subpaths ->
                    subpaths

                Err _ ->
                    []

        parsedTransform =
            transform |> Maybe.map parseTransform |> Maybe.withDefault identityTransform
    in
    { subpaths = parsedPath
    , transform = parsedTransform
    }


processXML model content =
    let
        xmlParse =
            XmlParser.parse content
    in
    case xmlParse of
        Ok { processingInstructions, docType, root } ->
            case root of
                XmlParser.Element tag attributes children ->
                    let
                        _ =
                            Debug.log "paths" pathInfos

                        pathNodes =
                            root
                                |> getAllXmlTags
                                |> List.filter (\( t, _ ) -> t == "path")

                        pathInfos : List PathInfo
                        pathInfos =
                            -- This works.
                            pathNodes
                                |> List.map
                                    (\( _, node ) ->
                                        { d = node |> getAttribute "d"
                                        , transform = node |> getAttribute "transform"
                                        }
                                    )

                        untransformedPaths =
                            pathInfos |> List.map parsePathInfo

                        untransformedPoints =
                            -- Now back where we were, add in the transforms.
                            untransformedPaths
                                |> List.map (.subpaths >> convertToPoints)
                                |> List.concat
                                |> List.map flipY

                        trackPoints =
                            untransformedPoints
                                |> List.map TrackPoint.trackPointFromPoint
                                |> TrackPoint.prepareTrackPoints

                        newTrack =
                            { trackPoints = trackPoints
                            , trackName = Just model.svgFilename
                            , currentNode =
                                List.head trackPoints
                                    |> Maybe.withDefault (TrackPoint.trackPointFromPoint Point3d.origin)
                            , markedNode = Nothing
                            , graph = Nothing
                            , earthReferenceCoordinates = ( 0.0, 0.0, 0.0 )
                            , box =
                                untransformedPoints
                                    |> BoundingBox3d.hullN
                                    |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
                            }
                    in
                    ( { model | track = Just newTrack }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


getAllXmlTags : XmlParser.Node -> List ( String, XmlParser.Node )
getAllXmlTags node =
    case node of
        XmlParser.Element tag attributes children ->
            ( tag, node )
                :: List.concatMap getAllXmlTags children

        Text string ->
            []


getAttribute : String -> XmlParser.Node -> Maybe String
getAttribute attribute node =
    case node of
        XmlParser.Element _ attributes _ ->
            attributes
                |> List.Extra.find
                    (\{ name, value } -> name == attribute)
                |> Maybe.map .value

        Text _ ->
            Nothing


view : (Msg -> msg) -> Element msg
view wrap =
    Input.button
        prettyButtonStyles
        { onPress = Just (wrap ReadFile)
        , label = text "Extract paths from SVG file"
        }


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


flipY : Point3d Meters LocalCoords -> Point3d Meters LocalCoords
flipY =
    Point3d.mirrorAcross Plane3d.zx


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
                        , startPoint = newPoint
                        , outputs = newPoint :: state.outputs
                    }

                MoveTo Relative ( dx, dy ) ->
                    let
                        newPoint =
                            state.currentPoint
                                |> Point3d.translateBy (Vector3d.meters dx dy 0.0)
                    in
                    { state
                        | currentPoint = newPoint
                        , startPoint = newPoint
                        , outputs = newPoint :: state.outputs
                    }
    in
    List.foldl drawCommand subPathState sub.drawtos


drawCommand : DrawTo -> PathState -> PathState
drawCommand command state =
    --TODO: ClosePath.
    --let
    --    _ =
    --        Debug.log "Command" command
    --in
    case command of
        LineTo Absolute points ->
            let
                initialState =
                    -- Current point and list of outputs
                    ( state.currentPoint, [] )

                finalState =
                    List.foldl absoluteLine initialState points

                absoluteLine ( x, y ) ( lastPoint, outputs ) =
                    let
                        nextPoint =
                            Point3d.meters x y 0.0
                    in
                    ( nextPoint, nextPoint :: outputs )
            in
            { state
                | currentPoint = Tuple.first finalState
                , outputs = Tuple.second finalState ++ state.outputs
            }

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

        Horizontal Absolute xs ->
            let
                pairs =
                    xs |> List.map (\x -> ( x, 0 ))
            in
            drawCommand (LineTo Absolute pairs) state

        Horizontal Relative dxs ->
            let
                pairs =
                    dxs |> List.map (\dx -> ( dx, 0 ))
            in
            drawCommand (LineTo Relative pairs) state

        Vertical Absolute ys ->
            let
                pairs =
                    ys |> List.map (\y -> ( 0, y ))
            in
            drawCommand (LineTo Absolute pairs) state

        Vertical Relative dys ->
            let
                pairs =
                    dys |> List.map (\dy -> ( 0, dy ))
            in
            drawCommand (LineTo Relative pairs) state

        CurveTo Relative triples ->
            let
                curveRelativeInitialState =
                    ( state.currentPoint, [] )

                curveRelativeFinalState =
                    List.foldl curveRelative curveRelativeInitialState triples

                curveRelative ( ( dx1, dy1 ), ( dx2, dy2 ), ( dxN, dyN ) ) ( lastPoint, outputs ) =
                    let
                        ( c1, c2, cN ) =
                            ( lastPoint |> Point3d.translateBy (Vector3d.meters dx1 dy1 0.0)
                            , lastPoint |> Point3d.translateBy (Vector3d.meters dx2 dy2 0.0)
                            , lastPoint |> Point3d.translateBy (Vector3d.meters dxN dyN 0.0)
                            )

                        spline =
                            CubicSpline3d.fromControlPoints lastPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( cN, splinePoints ++ outputs )
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
                            CubicSpline3d.fromControlPoints lastPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( cN, splinePoints ++ outputs )
            in
            { state
                | currentPoint = Tuple.first curveAbsoluteFinalState
                , outputs =
                    Tuple.second curveAbsoluteFinalState
                        ++ state.outputs
            }

        ClosePath ->
            { state
                | currentPoint = state.startPoint
                , outputs = state.startPoint :: state.outputs
            }

        _ ->
            state
