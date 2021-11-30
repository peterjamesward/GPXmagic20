module SceneBuilderProfile exposing (..)

import Color exposing (Color, black, brown)
import Cone3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Graph exposing (Graph)
import Length exposing (Length, Meters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import MoveAndStretch
import Pixels
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, cone)
import Scene3d.Material as Material exposing (Material)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Triangle3d
import Utils exposing (combineLists, gradientColourPastel, gradientColourVivid)
import Vector3d


type alias Scene =
    List (Entity LocalCoords)


type alias RenderingContext =
    -- The information we need to turn a track point list into a scene.
    { detailLevel : Float
    , trackDistanceInFocus : Quantity Float Length.Meters
    }


defaultRenderingContext =
    { detailLevel = 1.0
    , trackDistanceInFocus = Quantity.zero
    }


renderTrack : DisplayOptions -> Track -> Scene
renderTrack options track =
    let
        mapOverPairs f =
            List.map2 f track.trackPoints (List.drop 1 track.trackPoints)

        mapOverPoints f =
            List.map f track.trackPoints

        gradientFunction =
            case options.curtainStyle of
                NoCurtain ->
                    always Color.lightGray

                PlainCurtain ->
                    always <| Color.rgb255 0 100 0

                RainbowCurtain ->
                    gradientColourVivid

                PastelCurtain ->
                    gradientColourPastel
    in
    List.concat
        [ mapOverPairs (paintCurtainBetween options.verticalExaggeration gradientFunction)
        , if options.roadPillars then
            mapOverPoints (roadSupportPillar options.verticalExaggeration)

          else
            []
        , if options.roadCones then
            mapOverPoints (trackPointCone options.verticalExaggeration)

          else
            []
        ]


paintCurtainBetween : Float -> (Float -> Color) -> TrackPoint -> TrackPoint -> Entity LocalCoords
paintCurtainBetween scale colouring pt1 pt2 =
    let
        gradient =
            gradientFromPoint pt1
    in
    Scene3d.quad (Material.color <| colouring gradient)
        (scaledXZ scale pt1)
        (scaledXZ scale pt2)
        (Point3d.projectOnto Plane3d.xy pt2.profileXZ)
        (Point3d.projectOnto Plane3d.xy pt1.profileXZ)


scaledXZ : Float -> TrackPoint -> Point3d Meters LocalCoords
scaledXZ verticalExaggeration p =
    let
        { x, y, z } =
            Point3d.toMeters p.profileXZ
    in
    Point3d.fromTuple meters ( x, y, z * verticalExaggeration )


renderMarkers : DisplayOptions -> Track -> Scene
renderMarkers options track =
    let
        currentPositionDisc point =
            let
                lollipopAt =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 1.0)
                        (scaledXZ options.verticalExaggeration point)
            in
            Utils.lollipop lollipopAt Color.lightOrange

        markedNode point =
            let
                lollipopAt =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 1.0)
                        (scaledXZ options.verticalExaggeration point)
            in
            Utils.lollipop lollipopAt Color.purple
    in
    currentPositionDisc track.currentNode
        ++ (Maybe.map markedNode track.markedNode |> Maybe.withDefault [])


paintSomethingBetween scale width material pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from
                (scaledXZ scale pt1)
                (scaledXZ scale pt2)

        ( topEdgeVector, bottomEdgeVector ) =
            ( Vector3d.fromTuple meters ( 0.0, 0.0, width )
            , Vector3d.fromTuple meters ( 0.0, 0.0, -1 * width )
            )

        ( topEdge, bottomEdge ) =
            ( LineSegment3d.translateBy topEdgeVector roadAsSegment
            , LineSegment3d.translateBy bottomEdgeVector roadAsSegment
            )
    in
    [ Scene3d.quad material
        (LineSegment3d.startPoint topEdge)
        (LineSegment3d.endPoint topEdge)
        (LineSegment3d.endPoint bottomEdge)
        (LineSegment3d.startPoint bottomEdge)
    ]


simpleSelectiveDetail : RenderingContext -> Track -> List TrackPoint
simpleSelectiveDetail context track =
    -- Try reducing detail beyond region 1km either side of focus.
    let
        centreOfDetail =
            track.currentNode.distanceFromStart

        isCentral tp =
            (tp.distanceFromStart
                |> Quantity.greaterThan
                    (Quantity.minus (Length.kilometers 1.0) centreOfDetail)
            )
                && (tp.distanceFromStart
                        |> Quantity.lessThan
                            (Quantity.plus (Length.kilometers 1.0) centreOfDetail)
                   )

        precedingTrack =
            List.Extra.takeWhile (not << isCentral) track.trackPoints

        tailSection =
            List.Extra.takeWhileRight (not << isCentral) track.trackPoints

        detailSection =
            track.trackPoints
                |> List.Extra.dropWhile (not << isCentral)
                |> List.Extra.takeWhile isCentral

        sampledPreceding =
            precedingTrack |> takeAlternate |> takeAlternate

        sampledTail =
            tailSection |> takeAlternate |> takeAlternate
    in
    sampledPreceding ++ detailSection ++ sampledTail


takeAlternate : List a -> List a
takeAlternate source =
    let
        helper aa accum =
            case aa of
                a :: b :: c ->
                    helper c (a :: accum)

                [ a ] ->
                    helper [] (a :: accum)

                [] ->
                    List.reverse accum
    in
    helper source []


showGraphNodes : Graph -> List (Entity LocalCoords)
showGraphNodes graph =
    let
        shiftUp =
            -- Just 1mm above the road surface
            Vector3d.fromMeters { x = 0, y = 0, z = 0.001 }

        makeNode node =
            cone (Material.color Color.lightRed) <|
                Cone3d.startingAt
                    (Point3d.translateBy shiftUp node)
                    negativeZ
                    { radius = meters 5.0
                    , length = meters 5.0
                    }
    in
    graph |> Graph.nodePointList |> List.map makeNode


previewNudge : DisplayOptions -> List TrackPoint -> List (Entity LocalCoords)
previewNudge options points =
    let
        nudgeElement tp1 tp2 =
            paintSomethingBetween
                options.verticalExaggeration
                0.5
                (Material.matte Color.darkGrey)
                tp1
                tp2
    in
    List.concat <|
        List.map2
            nudgeElement
            points
            (List.drop 1 points)


previewMoveAndStretch : DisplayOptions -> List TrackPoint -> List (Entity LocalCoords)
previewMoveAndStretch options points =
    let
        element tp1 tp2 =
            paintSomethingBetween
                options.verticalExaggeration
                0.5
                (Material.matte Color.blue)
                tp1
                tp2
    in
    List.concat <|
        List.map2
            element
            points
            (List.drop 1 points)


roadSupportPillar : Float -> TrackPoint -> Entity LocalCoords
roadSupportPillar scale pt =
    let
        centre =
            LineSegment3d.from
                (scaledXZ scale pt)
                (scaledXZ scale pt |> Point3d.projectOnto Plane3d.xy)
    in
    Scene3d.lineSegment (Material.color brown) centre


trackPointCone : Float -> TrackPoint -> Entity LocalCoords
trackPointCone scale pt =
    -- V2.0 just uses crossed triangle.
    Scene3d.point
        { radius = Pixels.pixels 1 }
        (Material.color black)
        (scaledXZ scale pt)


previewProfileLine : DisplayOptions -> Color.Color -> List TrackPoint -> List (Entity LocalCoords)
previewProfileLine options color points =
    let
        material =
            Material.color color

        preview p1 p2 =
            paintSomethingBetween
                options.verticalExaggeration
                0.5
                material
                p1
                p2
    in
    List.map2 preview points (List.drop 1 points) |> combineLists


highlightPointsProfile : Color.Color -> List TrackPoint -> List (Entity LocalCoords)
highlightPointsProfile color points =
    let
        material =
            Material.color color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 5 } material p.profileXZ
    in
    List.map highlightPoint points
