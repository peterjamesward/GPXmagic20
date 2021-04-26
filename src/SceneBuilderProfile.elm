module SceneBuilderProfile exposing (..)

import Angle
import Axis3d
import Color
import Cone3d
import Direction3d exposing (negativeZ)
import DisplayOptions exposing (DisplayOptions)
import Graph exposing (Graph)
import Length exposing (Length, Meters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, cone)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (gradientColourPastel)
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
    List.map2
        (paintCurtainBetween options.verticalExaggeration)
        track.track
        (List.drop 1 track.track)


paintCurtainBetween : Float -> TrackPoint -> TrackPoint -> Entity LocalCoords
paintCurtainBetween scale pt1 pt2 =
    let
        scaledXZ : TrackPoint -> Point3d Meters LocalCoords
        scaledXZ p =
            let
                { x, y, z } =
                    Point3d.toMeters p.profileXZ
            in
            Point3d.fromTuple meters ( x, y, z * scale )

        gradient =
            Direction3d.from pt1.profileXZ pt2.profileXZ
                |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
                |> Maybe.withDefault Quantity.zero
    in
    Scene3d.quad (Material.color <| gradientColourPastel gradient)
        (scaledXZ pt1)
        (scaledXZ pt2)
        (Point3d.projectOnto Plane3d.xy pt2.profileXZ)
        (Point3d.projectOnto Plane3d.xy pt1.profileXZ)


renderMarkers : DisplayOptions -> Track -> Scene
renderMarkers options track =
    let
        scaledXZ : TrackPoint -> Point3d Meters LocalCoords
        scaledXZ p =
            let
                { x, y, z } =
                    Point3d.toMeters p.profileXZ
            in
            Point3d.fromTuple meters ( x, y, z * options.verticalExaggeration )

        currentPositionDisc point =
            [ cone (Material.color Color.lightOrange) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 10.1)
                        (scaledXZ point)
                    )
                    negativeZ
                    { radius = meters <| 3.0
                    , length = meters <| 10.0
                    }
            ]

        markedNode point =
            [ cone (Material.color Color.purple) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 10.1)
                        (scaledXZ point)
                    )
                    negativeZ
                    { radius = meters <| 3.5
                    , length = meters <| 8.0
                    }
            ]
    in
    currentPositionDisc track.currentNode
        ++ (Maybe.map markedNode track.markedNode |> Maybe.withDefault [])



paintSomethingBetween scale width material pt1 pt2 =
    let
        scaledXZ : TrackPoint -> Point3d Meters LocalCoords
        scaledXZ p =
            let
                { x, y, z } =
                    Point3d.toMeters p.profileXZ
            in
            Point3d.fromTuple meters ( x, y, z * scale )

        roadAsSegment =
            LineSegment3d.from
                (scaledXZ pt1)
                (scaledXZ pt2)

        ( topEdgeVector, bottomEdgeVector ) =
            ( Vector3d.fromTuple meters (0.0, 0.0, width)
            , Vector3d.fromTuple meters (0.0, 0.0, -1 * width)
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
            List.Extra.takeWhile (not << isCentral) track.track

        tailSection =
            List.Extra.takeWhileRight (not << isCentral) track.track

        detailSection =
            track.track
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
