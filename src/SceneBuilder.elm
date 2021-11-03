module SceneBuilder exposing (..)

import Angle exposing (Angle)
import Axis3d
import BoundingBox2d
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, brown, darkGreen, green)
import Cone3d
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (CurtainStyle(..), DisplayOptions)
import Graph exposing (Graph)
import Length exposing (Length, Meters, inMeters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d
import SpatialIndex exposing (SpatialNode(..))
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Triangle3d
import Utils exposing (gradientColourPastel, gradientColourVivid, squareAspect, terrainColourFromHeight)
import Vector3d


type alias RenderingContext =
    -- The information we need to turn a track point list into a scene.
    { detailLevel : Float
    , trackDistanceInFocus : Quantity Float Length.Meters
    }


roadWidth =
    Length.meters 4.0


defaultRenderingContext =
    { detailLevel = 1.0
    , trackDistanceInFocus = Quantity.zero
    }


renderTrack : DisplayOptions -> Track -> Scene
renderTrack options track =
    --TODO: Implement selective detail (optimisation!) = simpleSelectiveDetail context track
    let
        box =
            track.box
                |> BoundingBox3d.expandBy (meters 500)
                |> squareAspect

        terrain =
            if options.terrainOn then
                makeTerrain
                    options
                    track.box
                    (List.map .xyz track.trackPoints)

            else
                []

        reducedTrack =
            track.trackPoints

        trackShifted =
            List.drop 1 reducedTrack

        mapOverPairs f =
            List.concat <| List.map2 f reducedTrack trackShifted

        mapOverPoints f =
            List.concatMap f reducedTrack

        graphNodes =
            Maybe.map showGraphNodes track.graph |> Maybe.withDefault []

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

        scene =
            List.concat
                [ graphNodes
                , if options.centreLine then
                    mapOverPairs (centreLineBetween gradientColourPastel)

                  else
                    []
                , if options.roadTrack then
                    mapOverPairs paintSurfaceBetween

                  else
                    []
                , if options.curtainStyle /= NoCurtain && not options.terrainOn then
                    mapOverPairs (curtainBetween gradientFunction)

                  else if options.terrainOn then
                    mapOverPairs sidewall

                  else
                    []
                , if options.roadPillars && not options.terrainOn then
                    mapOverPoints roadSupportPillar

                  else
                    []
                , if options.roadCones && not options.terrainOn then
                    mapOverPoints trackPointCone

                  else
                    []
                , seaLevel options.seaLevel track.box
                , terrain
                ]
    in
    scene


renderMarkers : Maybe TrackPoint -> Track -> Scene
renderMarkers stretchMarker track =
    let
        currentPositionDisc point =
            [ cone (Material.color Color.lightOrange) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 20.1)
                        point.xyz
                    )
                    negativeZ
                    { radius = meters <| 6.0
                    , length = meters <| 20.0
                    }
            ]

        markedNode point =
            [ cone (Material.color Color.purple) <|
                Cone3d.startingAt
                    (Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 18.1)
                        point.xyz
                    )
                    negativeZ
                    { radius = meters <| 7.0
                    , length = meters <| 18.0
                    }
            ]

        stretch =
            case stretchMarker of
                Just pt ->
                    [ cone (Material.color Color.white) <|
                        Cone3d.startingAt
                            (Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 18.1)
                                pt.xyz
                            )
                            negativeZ
                            { radius = meters <| 5.0
                            , length = meters <| 22.0
                            }
                    ]

                Nothing ->
                    []
    in
    currentPositionDisc track.currentNode
        ++ (Maybe.map markedNode track.markedNode |> Maybe.withDefault [])
        ++ stretch


paintSurfaceBetween : TrackPoint -> TrackPoint -> List (Entity LocalCoords)
paintSurfaceBetween pt1 pt2 =
    paintSomethingBetween roadWidth (Material.matte Color.grey) pt1 pt2


centreLineBetween : (Float -> Color) -> TrackPoint -> TrackPoint -> List (Entity LocalCoords)
centreLineBetween colouring pt1 pt2 =
    let
        gradient =
            gradientFromPoint pt1

        smallUpshiftTo pt =
            -- To make line stand slightly proud of the road
            { pt | xyz = pt.xyz |> Point3d.translateBy (Vector3d.meters 0.0 0.0 0.005) }
    in
    paintSomethingBetween
        (Length.meters 0.5)
        (Material.color <| colouring gradient)
        (smallUpshiftTo pt1)
        (smallUpshiftTo pt2)


curtainBetween : (Float -> Color) -> TrackPoint -> TrackPoint -> List (Entity LocalCoords)
curtainBetween colouring pt1 pt2 =
    let
        gradient =
            gradientFromPoint pt1

        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2.xyz

        curtainHem =
            roadAsSegment |> LineSegment3d.projectOnto Plane3d.xy
    in
    [ Scene3d.quad (Material.color <| colouring gradient)
        (LineSegment3d.startPoint roadAsSegment)
        (LineSegment3d.endPoint roadAsSegment)
        (LineSegment3d.endPoint curtainHem)
        (LineSegment3d.startPoint curtainHem)
    ]


sidewall : TrackPoint -> TrackPoint -> List (Entity LocalCoords)
sidewall pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2.xyz

        halfWidth =
            Vector3d.from pt1.xyz pt2.xyz
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo roadWidth

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( LineSegment3d.translateBy leftKerbVector roadAsSegment
            , LineSegment3d.translateBy rightKerbVector roadAsSegment
            )

        ( leftFooting, rightFooting ) =
            ( leftKerb |> LineSegment3d.projectOnto Plane3d.xy
            , rightKerb |> LineSegment3d.projectOnto Plane3d.xy
            )

        colour =
            terrainColourFromHeight <| inMeters <| Point3d.zCoordinate <| pt1.xyz
    in
    [ Scene3d.quad (Material.matte colour)
        (LineSegment3d.startPoint leftKerb)
        (LineSegment3d.endPoint leftKerb)
        (LineSegment3d.endPoint leftFooting)
        (LineSegment3d.startPoint leftFooting)
    , Scene3d.quad (Material.matte colour)
        (LineSegment3d.startPoint rightKerb)
        (LineSegment3d.endPoint rightKerb)
        (LineSegment3d.endPoint rightFooting)
        (LineSegment3d.startPoint rightFooting)
    ]


paintSomethingBetween width material pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2.xyz

        halfWidth =
            Vector3d.from pt1.xyz pt2.xyz
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


roadSupportPillar : TrackPoint -> List (Entity LocalCoords)
roadSupportPillar pt =
    -- V2.0 just uses an extruded cross.
    let
        centre =
            LineSegment3d.from
                (pt.xyz |> Point3d.translateBy (Vector3d.meters 0.0 0.0 -1.0))
                (pt.xyz |> Point3d.projectOnto Plane3d.xy)

        eastSide =
            centre |> LineSegment3d.translateBy (Vector3d.meters -1.0 0.0 0.0)

        westSide =
            centre |> LineSegment3d.translateBy (Vector3d.meters 1.0 0.0 0.0)

        northSide =
            centre |> LineSegment3d.translateBy (Vector3d.meters 0.0 1.0 0.0)

        southSide =
            centre |> LineSegment3d.translateBy (Vector3d.meters 0.0 -1.0 0.0)
    in
    [ Scene3d.quad (Material.color brown)
        (LineSegment3d.startPoint eastSide)
        (LineSegment3d.endPoint eastSide)
        (LineSegment3d.endPoint westSide)
        (LineSegment3d.startPoint westSide)
    , Scene3d.quad (Material.color brown)
        (LineSegment3d.startPoint northSide)
        (LineSegment3d.endPoint northSide)
        (LineSegment3d.endPoint southSide)
        (LineSegment3d.startPoint southSide)
    ]


trackPointCone : TrackPoint -> List (Entity LocalCoords)
trackPointCone pt =
    -- V2.0 just uses crossed triangle.
    let
        eastSide =
            pt.xyz |> Point3d.translateBy (Vector3d.meters -1.0 0.0 -1.0)

        westSide =
            pt.xyz |> Point3d.translateBy (Vector3d.meters 1.0 0.0 -1.0)

        northSide =
            pt.xyz |> Point3d.translateBy (Vector3d.meters 0.0 1.0 -1.0)

        southSide =
            pt.xyz |> Point3d.translateBy (Vector3d.meters 0.0 -1.0 -1.0)
    in
    [ Scene3d.triangle (Material.color black)
        (Triangle3d.fromVertices ( eastSide, pt.xyz, westSide ))
    , Scene3d.triangle (Material.color black)
        (Triangle3d.fromVertices ( northSide, pt.xyz, southSide ))
    ]


seaLevel : Bool -> BoundingBox3d Meters LocalCoords -> List (Entity LocalCoords)
seaLevel useActualWaterLevel box =
    -- For seaLevel, True means elevation 0, False means just below minZ
    let
        groundZ =
            if useActualWaterLevel then
                meters 0.0

            else
                BoundingBox3d.minZ box |> Quantity.minus (meters 10.0)

        { minX, maxX, minY, maxY, minZ, maxZ } =
            box
                |> BoundingBox3d.expandBy (meters 500)
                |> squareAspect
                |> BoundingBox3d.extrema
    in
    [ Scene3d.quad (Material.color Color.darkGreen)
        (Point3d.xyz minX minY groundZ)
        (Point3d.xyz minX maxY groundZ)
        (Point3d.xyz maxX maxY groundZ)
        (Point3d.xyz maxX minY groundZ)
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


previewMoveAndStretch : List TrackPoint -> List (Entity LocalCoords)
previewMoveAndStretch points =
    let
        nudgeElement tp1 tp2 =
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.blue) tp1 tp2
    in
    List.concat <|
        List.map2
            nudgeElement
            points
            (List.drop 1 points)


previewBend : List TrackPoint -> List (Entity LocalCoords)
previewBend points =
    let
        section tp1 tp2 =
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.lightYellow) tp1 tp2
    in
    List.concat <|
        List.map2
            section
            points
            (List.drop 1 points)


previewStravaSegment : List TrackPoint -> List (Entity LocalCoords)
previewStravaSegment points =
    List.concat <|
        List.map2
            (paintSomethingBetween (Length.meters 3.0) (Material.matte Color.purple))
            points
            (List.drop 1 points)


arrowhead colour pt1 pt2 =
    let
        halfWidth =
            Vector3d.from pt1.xyz pt2.xyz
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo (meters 5.0)

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( pt1.xyz |> Point3d.translateBy leftKerbVector
            , pt1.xyz |> Point3d.translateBy rightKerbVector
            )
    in
    Triangle3d.fromVertices ( leftKerb, rightKerb, pt2.xyz )
        |> Triangle3d.translateBy (Vector3d.meters 0 0 0.3)
        |> Scene3d.triangle (Material.color colour)


showGraphEdge : List TrackPoint -> List (Entity LocalCoords)
showGraphEdge points =
    List.map2
        (arrowhead Color.blue)
        points
        (List.drop 1 points)


type alias Point =
    Point3d.Point3d Length.Meters LocalCoords


type alias Line =
    LineSegment3d.LineSegment3d Length.Meters LocalCoords


type alias IndexEntry =
    { content : Quantity Float Meters
    , box : BoundingBox2d.BoundingBox2d Meters LocalCoords
    }


type alias Index =
    SpatialIndex.SpatialNode (Quantity Float Meters) Length.Meters LocalCoords


flatBox box3d =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box3d
    in
    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }


makeTerrain :
    DisplayOptions
    -> BoundingBox3d Length.Meters LocalCoords
    -> List (Point3d Length.Meters LocalCoords)
    -> List (Entity LocalCoords)
makeTerrain options box points =
    indexTerrain box points
        |> terrainFromIndex (flatBox box) options.terrainFineness (BoundingBox3d.minZ box)


indexTerrain :
    BoundingBox3d Length.Meters LocalCoords
    -> List (Point3d Length.Meters LocalCoords)
    -> Index
indexTerrain box points =
    let
        emptyIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty (flatBox box) (Length.meters 10.0)

        makeRoadBox pt1 pt2 =
            let
                halfWidthVector =
                    Vector3d.from pt1 pt2
                        |> Vector3d.projectOnto Plane3d.xy
                        |> Vector3d.scaleTo roadWidth

                ( leftKerbVector, rightKerbVector ) =
                    ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidthVector
                    , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidthVector
                    )

                ( leftNearKerb, rightNearKerb ) =
                    ( Point3d.translateBy leftKerbVector pt1
                    , Point3d.translateBy rightKerbVector pt1
                    )

                ( leftFarKerb, rightFarKerb ) =
                    ( Point3d.translateBy leftKerbVector pt2
                    , Point3d.translateBy rightKerbVector pt2
                    )
            in
            BoundingBox3d.hull leftNearKerb [ leftFarKerb, rightFarKerb, rightNearKerb ]

        tarmac =
            List.map2
                makeRoadBox
                points
                (List.drop 1 points)

        indexContent =
            List.map
                (\abox ->
                    { content = abox |> BoundingBox3d.minZ
                    , box = flatBox abox
                    }
                )
                tarmac

        indexedContent =
            List.foldl SpatialIndex.add emptyIndex indexContent
    in
    indexedContent


terrainFromIndex :
    BoundingBox2d.BoundingBox2d Length.Meters LocalCoords
    -> Int
    -> Quantity Float Meters
    -> SpatialNode (Quantity Float Meters) Meters LocalCoords
    -> List (Entity LocalCoords)
terrainFromIndex box fineness base index =
    let
        thisQuery =
            { content = Quantity.zero
            , box = box
            }

        content =
            SpatialIndex.query index thisQuery

        top =
            content
                |> List.map .content
                |> Quantity.minimum
                |> Maybe.withDefault Quantity.negativeInfinity
                |> Quantity.minus (meters 0.1)

        extrema =
            BoundingBox2d.extrema box

        centre =
            BoundingBox2d.centerPoint box

        actualBox =
            -- This box encloses our points. It defines the top of the frustrum and the base for the next level.
            content
                |> BoundingBox2d.aggregateOfN .box
                |> Maybe.withDefault box
                |> BoundingBox2d.intersection box
                |> Maybe.withDefault box

        innerExtrema =
            BoundingBox2d.extrema actualBox

        { nwChildBox, neChildBox, swChildBox, seChildBox } =
            { nwChildBox = BoundingBox2d.from centre (Point2d.xy extrema.minX extrema.maxY)
            , neChildBox = BoundingBox2d.from centre (Point2d.xy extrema.maxX extrema.maxY)
            , swChildBox = BoundingBox2d.from centre (Point2d.xy extrema.minX extrema.minY)
            , seChildBox = BoundingBox2d.from centre (Point2d.xy extrema.maxX extrema.minY)
            }

        isNotTiny bx =
            let
                ( width, height ) =
                    BoundingBox2d.dimensions bx

                splitSize =
                    meters <| toFloat fineness
            in
            (width |> Quantity.greaterThan splitSize)
                && (height |> Quantity.greaterThan splitSize)

        topColour =
            terrainColourFromHeight <| inMeters top

        sideColour =
            terrainColourFromHeight <| 0.5 * (inMeters top + inMeters base)

        thisLevelSceneElements =
            [ Scene3d.quad (Material.matte topColour)
                (Point3d.xyz innerExtrema.minX innerExtrema.minY top)
                (Point3d.xyz innerExtrema.minX innerExtrema.maxY top)
                (Point3d.xyz innerExtrema.maxX innerExtrema.maxY top)
                (Point3d.xyz innerExtrema.maxX innerExtrema.minY top)
            , Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz innerExtrema.minX innerExtrema.minY top)
                (Point3d.xyz innerExtrema.minX innerExtrema.maxY top)
                (Point3d.xyz extrema.minX extrema.maxY base)
                (Point3d.xyz extrema.minX extrema.minY base)
            , Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz innerExtrema.maxX innerExtrema.minY top)
                (Point3d.xyz innerExtrema.maxX innerExtrema.maxY top)
                (Point3d.xyz extrema.maxX extrema.maxY base)
                (Point3d.xyz extrema.maxX extrema.minY base)
            , Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz innerExtrema.minX innerExtrema.minY top)
                (Point3d.xyz innerExtrema.maxX innerExtrema.minY top)
                (Point3d.xyz extrema.maxX extrema.minY base)
                (Point3d.xyz extrema.minX extrema.minY base)
            , Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz innerExtrema.minX innerExtrema.maxY top)
                (Point3d.xyz innerExtrema.maxX innerExtrema.maxY top)
                (Point3d.xyz extrema.maxX extrema.maxY base)
                (Point3d.xyz extrema.minX extrema.maxY base)
            ]
    in
    thisLevelSceneElements
        ++ -- No point recursing if one element only.
           (if List.length content > 1 && isNotTiny box then
                terrainFromIndex nwChildBox fineness top index
                    ++ terrainFromIndex neChildBox fineness top index
                    ++ terrainFromIndex seChildBox fineness top index
                    ++ terrainFromIndex swChildBox fineness top index

            else
                []
           )



-- END
