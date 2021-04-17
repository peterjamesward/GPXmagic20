module SceneBuilderProfile exposing (..)

import Angle
import Axis3d
import Color
import Cone3d
import Direction3d exposing (negativeZ)
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


renderTrack : RenderingContext -> Track -> Scene
renderTrack context track =
    -- Let's just try a clean room implementation here, with surface only.
    let
        reducedTrack =
            track.track

        --simpleSelectiveDetail context track
    in
    --(Maybe.map showGraphNodes track.graph |> Maybe.withDefault []) ++
    List.map2
        paintCurtainBetween
        reducedTrack
        (List.drop 1 reducedTrack)


paintCurtainBetween : TrackPoint -> TrackPoint -> Entity LocalCoords
paintCurtainBetween pt1 pt2 =
    let
        gradient =
            Direction3d.from pt1.profileXZ pt2.profileXZ
                |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
                |> Maybe.withDefault Quantity.zero
    in
    Scene3d.quad (Material.color <| gradientColourPastel gradient)
        pt1.profileXZ
        pt2.profileXZ
        (Point3d.projectOnto Plane3d.xy pt2.profileXZ)
        (Point3d.projectOnto Plane3d.xy pt1.profileXZ)


renderMarkers : Track -> Scene
renderMarkers track =
    let
        currentPositionDisc point =
            [ cone (Material.color Color.lightOrange) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 10.1)
                        point.profileXZ
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
                        point.profileXZ
                    )
                    negativeZ
                    { radius = meters <| 3.5
                    , length = meters <| 8.0
                    }
            ]
    in
    currentPositionDisc track.currentNode
        ++ (Maybe.map markedNode track.markedNode |> Maybe.withDefault [])


paintSurfaceBetween : TrackPoint -> TrackPoint -> List (Entity LocalCoords)
paintSurfaceBetween pt1 pt2 =
    paintSomethingBetween (Length.meters 3.0) (Material.matte Color.grey) pt1 pt2


paintSomethingBetween width material pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1.profileXZ pt2.profileXZ

        halfWidth =
            Vector3d.from pt1.profileXZ pt2.profileXZ
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo width

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( LineSegment3d.translateBy leftKerbVector roadAsSegment
            , LineSegment3d.translateBy rightKerbVector roadAsSegment
            )
    in
    [ Scene3d.quad material
        (LineSegment3d.startPoint leftKerb)
        (LineSegment3d.endPoint leftKerb)
        (LineSegment3d.endPoint rightKerb)
        (LineSegment3d.startPoint rightKerb)
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


previewNudge : List TrackPoint -> List (Entity LocalCoords)
previewNudge points =
    let
        nudgeElement tp1 tp2 =
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.lightOrange) tp1 tp2
    in
    List.concat <|
        List.map2
            nudgeElement
            points
            (List.drop 1 points)
