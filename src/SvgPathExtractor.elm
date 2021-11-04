module SvgPathExtractor exposing (..)

import AltMath.Matrix4 as AltMath
import AltMath.Vector3
import BoundingBox2d
import BoundingBox3d
import CubicSpline3d
import Element exposing (Element, text)
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GpxParser exposing (asRegex)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Path.LowLevel exposing (DrawTo(..), Mode(..), MoveTo(..), SubPath)
import Path.LowLevel.Parser as PathParser
import Plane3d
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity(..))
import Regex
import SketchPlane3d
import SpatialIndex
import Task
import Track exposing (Track)
import TrackPoint
import Utils exposing (clickTolerance, flatBox)
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
parseTransform text =
    let
        value x =
            case x of
                Just val ->
                    String.toFloat val

                _ ->
                    Nothing

        hasScale =
            --TODO: Optional y scale value!
            text
                |> Regex.find (asRegex "scale\\((-?\\d*\\.?\\d*)\\)")
                |> List.map .submatches
                |> List.concat
                |> List.filterMap value
                |> List.head

        hasMatrix =
            text
                |> Regex.find (asRegex "matrix\\((-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*)\\)")
                |> List.map .submatches
                |> List.concat
                |> List.filterMap value

        applyScale baseMatrix =
            case hasScale of
                Just scale ->
                    baseMatrix |> AltMath.scale3 scale scale 1.0

                Nothing ->
                    baseMatrix

        applyMatrix baseMatrix =
            case hasMatrix of
                [ a, b, c, d, e, f ] ->
                    let
                        matrix =
                            -- Careful, it's a 4x4.
                            { identityTransform
                                | m11 = a
                                , m21 = b
                                , m12 = c
                                , m22 = d
                                , m14 = e
                                , m24 = f
                            }
                    in
                    baseMatrix |> AltMath.mul matrix

                _ ->
                    baseMatrix
    in
    identityTransform
        |> applyMatrix
        |> applyScale


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
            transform
                |> Maybe.map parseTransform
                |> Maybe.withDefault identityTransform
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
                        pathNodes =
                            root
                                |> getAllXmlTags
                                |> List.reverse
                                |> List.filter (\( t, _ ) -> t == "path")

                        pathInfos : List PathInfo
                        pathInfos =
                            pathNodes
                                |> List.map
                                    (\( _, node ) ->
                                        { d = node |> getAttribute "d"
                                        , transform = node |> getAttribute "transform"
                                        }
                                    )

                        untransformedPaths : List PathAndTransform
                        untransformedPaths =
                            pathInfos |> List.map parsePathInfo

                        pathState : PathState
                        pathState =
                            { startPoint = Point3d.origin
                            , currentPoint = Point3d.origin
                            , outputs = []
                            }

                        finalPathState =
                            -- Now back where we were, add in the transforms.
                            untransformedPaths
                                |> List.foldl convertToPoints pathState

                        trackPoints =
                            finalPathState.outputs
                                |> List.map TrackPoint.trackPointFromPoint
                                |> TrackPoint.prepareTrackPoints

                        newTrack =
                            let
                                box =
                                    BoundingBox3d.hullOfN .xyz trackPoints
                                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

                                emptyIndex =
                                    -- Large split threshold to avoid excessive depth.
                                    SpatialIndex.empty (flatBox box) (Length.meters 100.0)

                                index =
                                    List.foldl
                                        (\point ->
                                            SpatialIndex.add
                                                { content = point
                                                , box =
                                                    BoundingBox2d.withDimensions clickTolerance
                                                        (point.xyz |> Point3d.projectInto SketchPlane3d.xy)
                                                }
                                        )
                                        emptyIndex
                                        trackPoints
                            in
                            { trackName = Just model.svgFilename
                            , trackPoints = trackPoints
                            , currentNode =
                                List.head trackPoints
                                    |> Maybe.withDefault (TrackPoint.trackPointFromPoint Point3d.origin)
                            , markedNode = Nothing
                            , graph = Nothing
                            , earthReferenceCoordinates = ( 0.0, 0.0, 0.0 )
                            , box = box
                            , spatialIndex = index
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


convertToPoints : PathAndTransform -> PathState -> PathState
convertToPoints pathAndTransform pathState =
    -- We now fold this over the paths so need to track current point externally.
    let
        applyTransform : Point3d Meters LocalCoords -> Point3d Meters LocalCoords
        applyTransform before =
            before
                |> Point3d.toRecord inMeters
                |> AltMath.Vector3.fromRecord
                |> AltMath.transform pathAndTransform.transform
                |> AltMath.Vector3.toRecord
                |> Point3d.fromRecord meters

        localPathState =
            { pathState | outputs = [] }

        newLocalPathState =
            List.foldl followSubPath localPathState pathAndTransform.subpaths

        pointsFromThisPath =
            newLocalPathState
                |> .outputs
                |> List.map applyTransform
                |> List.map flipY
    in
    { pathState
        | currentPoint = pathState.currentPoint --applyTransform newLocalPathState.currentPoint
        , outputs = pathState.outputs ++ pointsFromThisPath
    }


flipY : Point3d Meters LocalCoords -> Point3d Meters LocalCoords
flipY =
    Point3d.mirrorAcross Plane3d.zx


followSubPath : SubPath -> PathState -> PathState
followSubPath sub state =
    let
        newPoint =
            case sub.moveto of
                MoveTo Absolute ( x, y ) ->
                    Point3d.meters x y 0.0

                MoveTo Relative ( dx, dy ) ->
                    state.currentPoint
                        |> Point3d.translateBy (Vector3d.meters dx dy 0.0)

        subPathState =
            { state
                | currentPoint = newPoint
                , startPoint = newPoint
                , outputs = [ newPoint ]
            }

        endSubPathState =
            List.foldl drawCommand subPathState sub.drawtos
    in
    { state
        | currentPoint = endSubPathState.currentPoint
        , outputs = state.outputs ++ List.reverse endSubPathState.outputs
    }


drawCommand : DrawTo -> PathState -> PathState
drawCommand command state =
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
