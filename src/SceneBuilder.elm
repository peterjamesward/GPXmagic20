module SceneBuilder exposing (..)

import Angle exposing (Angle)
import Axis3d
import BoundingBox2d exposing (BoundingBox2d)
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
import Maybe.Extra as Maybe
import MoveAndStretch
import Pixels
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import Scene3d exposing (Entity, cone, cylinder)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d
import SpatialIndex exposing (SpatialContent, SpatialNode(..))
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Triangle3d
import Utils exposing (combineLists, gradientColourPastel, gradientColourVivid, lollipop, reversingCons, squareAspect, terrainColourFromHeight)
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


highlightPoints : Color.Color -> List TrackPoint -> List (Entity LocalCoords)
highlightPoints color points =
    let
        material =
            Material.color color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 5 } material p.xyz
    in
    List.map highlightPoint points


highlightPointsProfile : Color.Color -> List TrackPoint -> List (Entity LocalCoords)
highlightPointsProfile color points =
    let
        material =
            Material.color color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 5 } material p.profileXZ
    in
    List.map highlightPoint points


renderTerrain : DisplayOptions -> Track -> Scene
renderTerrain options track =
    let
        box =
            track.box
                |> BoundingBox3d.expandBy (meters 500)
                |> squareAspect
    in
    seaLevel options.seaLevel box
        ++ makeTerrain
            options
            box
            track.trackPoints


renderTrack : DisplayOptions -> Track -> Scene
renderTrack options track =
    let
        box =
            track.box
                |> BoundingBox3d.expandBy (meters 500)
                |> squareAspect

        floorPlane =
            -- To draw the road curtains properly if below sea level.
            Plane3d.xy |> Plane3d.offsetBy (BoundingBox3d.minZ track.box)

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

        scenePainterFunctions : List (TrackPoint -> Scene)
        scenePainterFunctions =
            [ if options.centreLine then
                Just (centreLineBetween gradientColourPastel)

              else
                Nothing
            , if options.roadTrack then
                Just paintSurfaceBetween

              else
                Nothing
            , if options.curtainStyle /= NoCurtain && not options.terrainOn then
                Just (curtainBetween floorPlane gradientFunction)

              else
                Nothing
            , if options.roadPillars && not options.terrainOn then
                Just (roadSupportPillar floorPlane)

              else
                Nothing
            , if options.roadCones && not options.terrainOn then
                Just trackPointCone

              else
                Nothing
            ]
                |> List.filterMap identity

        paintScenePart : TrackPoint -> Scene -> Scene
        paintScenePart pt accum =
            -- This is intended to allow us to touch each TP once, create all the
            -- active scene elements, and build a scene list with minimal overhead.
            scenePainterFunctions
                |> List.map (\fn -> fn pt)
                |> combineLists
                |> reversingCons accum

        scene : Scene
        scene =
            List.foldl paintScenePart [] track.trackPoints
    in
    combineLists [ seaLevel options.seaLevel track.box, graphNodes, scene ]


renderMarkers : Maybe TrackPoint -> Track -> Scene
renderMarkers stretchMarker track =
    let
        currentPositionDisc point =
            lollipop point.xyz Color.lightOrange

        markedNode point =
            lollipop point.xyz Color.purple

        stretch =
            case stretchMarker of
                Just pt ->
                    lollipop pt.xyz Color.white

                Nothing ->
                    []
    in
    currentPositionDisc track.currentNode
        ++ (Maybe.map markedNode track.markedNode |> Maybe.withDefault [])
        ++ stretch


renderMRLimits : Track -> Scene
renderMRLimits isTrack =
    let
        orangeDistance =
            isTrack.currentNode.distanceFromStart

        renderingLimit =
            Length.kilometers 1.5

        farthestVisibleTrackPointInFront =
            isTrack.trackPoints
                |> List.Extra.takeWhile
                    (\pt ->
                        pt.distanceFromStart
                            |> Quantity.lessThanOrEqualTo
                                (orangeDistance |> Quantity.plus renderingLimit)
                    )
                |> List.Extra.last

        farthestVisibleTrackPointBehind =
            isTrack.trackPoints
                |> List.Extra.splitWhen
                    (\pt ->
                        pt.distanceFromStart
                            |> Quantity.greaterThanOrEqualTo
                                (orangeDistance |> Quantity.minus renderingLimit)
                    )
                |> Maybe.map (Tuple.first >> List.Extra.last)
                |> Maybe.join

        behindLimitLocation =
            case farthestVisibleTrackPointBehind of
                Just behindTP ->
                    behindTP.xyz
                        |> Point3d.translateBy
                            (behindTP.roadVector
                                |> Vector3d.scaleTo
                                    ((orangeDistance |> Quantity.minus renderingLimit)
                                        |> Quantity.minus behindTP.distanceFromStart
                                    )
                            )
                        |> Just

                Nothing ->
                    Nothing

        forwardLimitLocation =
            case farthestVisibleTrackPointInFront of
                Just forwardTP ->
                    forwardTP.xyz
                        |> Point3d.translateBy
                            (forwardTP.roadVector
                                |> Vector3d.scaleTo
                                    ((orangeDistance |> Quantity.plus renderingLimit)
                                        |> Quantity.minus forwardTP.distanceFromStart
                                    )
                            )
                        |> Just

                Nothing ->
                    Nothing

        showMarker location =
            case location of
                Just lollipopAt ->
                    lollipop lollipopAt Color.gray

                Nothing ->
                    []
    in
    showMarker forwardLimitLocation ++ showMarker behindLimitLocation


paintSurfaceBetween : TrackPoint -> List (Entity LocalCoords)
paintSurfaceBetween pt1 =
    let
        pt2 =
            pt1.xyz |> Point3d.translateBy pt1.roadVector
    in
    paintSomethingBetween roadWidth (Material.matte Color.grey) pt1.xyz pt2


centreLineBetween : (Float -> Color) -> TrackPoint -> List (Entity LocalCoords)
centreLineBetween colouring pt1 =
    let
        gradient =
            gradientFromPoint pt1

        smallUpshiftTo pt =
            -- To make line stand slightly proud of the road
            pt |> Point3d.translateBy (Vector3d.meters 0.0 0.0 0.005)

        pt2 =
            pt1.xyz |> Point3d.translateBy pt1.roadVector
    in
    paintSomethingBetween
        (Length.meters 0.5)
        (Material.color <| colouring gradient)
        (smallUpshiftTo pt1.xyz)
        (smallUpshiftTo pt2)


curtainBetween :
    Plane3d.Plane3d Length.Meters LocalCoords
    -> (Float -> Color)
    -> TrackPoint
    -> List (Entity LocalCoords)
curtainBetween floorPlane colouring pt1 =
    let
        gradient =
            gradientFromPoint pt1

        pt2 =
            pt1.xyz |> Point3d.translateBy pt1.roadVector

        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2

        curtainHem =
            LineSegment3d.projectOnto floorPlane roadAsSegment
    in
    [ Scene3d.quad (Material.color <| colouring gradient)
        (LineSegment3d.startPoint roadAsSegment)
        (LineSegment3d.endPoint roadAsSegment)
        (LineSegment3d.endPoint curtainHem)
        (LineSegment3d.startPoint curtainHem)
    ]


sidewall : Quantity Float Meters -> TrackPoint -> List (Entity LocalCoords)
sidewall baseElevation pt1 =
    let
        pt2 =
            pt1.xyz |> Point3d.translateBy pt1.roadVector

        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2

        halfWidth =
            Vector3d.from pt1.xyz pt2
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

        basePlane =
            Plane3d.xy |> Plane3d.translateBy (Vector3d.xyz Quantity.zero Quantity.zero baseElevation)

        ( leftFooting, rightFooting ) =
            ( leftKerb |> LineSegment3d.projectOnto basePlane
            , rightKerb |> LineSegment3d.projectOnto basePlane
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
            LineSegment3d.from pt1 pt2

        halfWidth =
            Vector3d.from pt1 pt2
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


roadSupportPillar :
    Plane3d.Plane3d Length.Meters LocalCoords
    -> TrackPoint
    -> List (Entity LocalCoords)
roadSupportPillar floorPlane pt =
    -- V2.0 just uses an extruded cross.
    let
        centre =
            LineSegment3d.from
                (pt.xyz |> Point3d.translateBy (Vector3d.meters 0.0 0.0 -1.0))
                (pt.xyz |> Point3d.projectOnto floorPlane)
    in
    [ Scene3d.lineSegment (Material.color brown) centre ]


trackPointCone : TrackPoint -> List (Entity LocalCoords)
trackPointCone pt =
    [ Scene3d.point { radius = Pixels.pixels 1 } (Material.color black) pt.xyz ]


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
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.lightOrange) tp1.xyz tp2.xyz
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
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.blue) tp1.xyz tp2.xyz
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
            paintSomethingBetween (Length.meters 1.0) (Material.matte Color.lightYellow) tp1.xyz tp2.xyz
    in
    List.concat <|
        List.map2
            section
            points
            (List.drop 1 points)


previewStravaSegment : List TrackPoint -> List (Entity LocalCoords)
previewStravaSegment trackpoints =
    let
        points =
            List.map .xyz trackpoints
    in
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
    { elevation : Quantity Float Meters
    , road : ( TrackPoint, TrackPoint )
    }


type alias Index =
    SpatialIndex.SpatialNode IndexEntry Length.Meters LocalCoords


flatBox box3d =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box3d
    in
    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }


makeTerrain :
    DisplayOptions
    -> BoundingBox3d Length.Meters LocalCoords
    -> List TrackPoint
    -> List (Entity LocalCoords)
makeTerrain options box points =
    let
        roads =
            List.foldl
                (paintTheRoad >> reversingCons)
                []
                points

        sidewallBase =
            BoundingBox3d.minZ box

        paintTheRoad pt1 =
            paintSurfaceBetween pt1
                ++ sidewall sidewallBase pt1
                ++ (if options.centreLine then
                        centreLineBetween gradientColourPastel pt1

                    else
                        []
                   )

        terrain =
            indexTerrain box points
                |> terrainFromIndex
                    (flatBox box)
                    (flatBox box)
                    NoContext
                    options
                    (BoundingBox3d.minZ box)
    in
    terrain ++ roads


indexTerrain :
    BoundingBox3d Length.Meters LocalCoords
    -> List TrackPoint
    -> Index
indexTerrain box points =
    let
        emptyIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty (flatBox box) (Length.meters 100.0)

        makeRoadBox pt1 pt2 =
            let
                halfWidthVector =
                    Vector3d.from pt1.xyz pt2.xyz
                        |> Vector3d.projectOnto Plane3d.xy
                        |> Vector3d.scaleTo roadWidth

                ( leftKerbVector, rightKerbVector ) =
                    ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidthVector
                    , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidthVector
                    )

                ( leftNearKerb, rightNearKerb ) =
                    ( Point3d.translateBy leftKerbVector pt1.xyz
                    , Point3d.translateBy rightKerbVector pt1.xyz
                    )

                ( leftFarKerb, rightFarKerb ) =
                    ( Point3d.translateBy leftKerbVector pt2.xyz
                    , Point3d.translateBy rightKerbVector pt2.xyz
                    )
            in
            { localBounds =
                BoundingBox3d.hull leftNearKerb [ leftFarKerb, rightFarKerb, rightNearKerb ]
            , road = ( pt1, pt2 )
            }

        tarmac =
            List.map2
                makeRoadBox
                points
                (List.drop 1 points)

        indexContent =
            List.map
                (\{ localBounds, road } ->
                    { content =
                        { elevation = localBounds |> BoundingBox3d.minZ
                        , road = road
                        }
                    , box = flatBox localBounds
                    }
                )
                tarmac

        indexedContent =
            List.foldl SpatialIndex.add emptyIndex indexContent
    in
    indexedContent


type LocationContext
    = NoContext
    | NW
    | NE
    | SE
    | SW


type alias TerrainFoldState =
    { minAltitude : Quantity Float Meters
    , resultBox : BoundingBox2d Meters LocalCoords
    , count : Int
    }


terrainFromIndex :
    BoundingBox2d.BoundingBox2d Length.Meters LocalCoords
    -> BoundingBox2d.BoundingBox2d Length.Meters LocalCoords
    -> LocationContext
    -> DisplayOptions
    -> Quantity Float Meters
    -> Index
    -> List (Entity LocalCoords)
terrainFromIndex myBox enclosingBox orientation options baseElevation index =
    -- I played with the idea of pushing some of this work into the SpatialIndex but
    -- concluded (rightly, I think) that it would be wrong, other than using the new queryWithFold
    -- function to bring back both the minimum altitude and the aggregate content bounding box
    -- for each query. That's a marginal saving but maybe easier to comprehend.
    -- (Turned out to be 7x speed uplift, so not exactly marginal.)
    let
        centre =
            BoundingBox2d.centerPoint myBox

        initialFoldState : TerrainFoldState
        initialFoldState =
            { minAltitude = Quantity.positiveInfinity
            , resultBox = BoundingBox2d.singleton centre
            , count = 0
            }

        queryFoldFunction :
            SpatialContent IndexEntry Meters LocalCoords
            -> TerrainFoldState
            -> TerrainFoldState
        queryFoldFunction entry accum =
            { minAltitude = Quantity.min accum.minAltitude entry.content.elevation
            , resultBox = BoundingBox2d.union accum.resultBox entry.box
            , count = accum.count + 1
            }

        { minAltitude, resultBox, count } =
            SpatialIndex.queryWithFold index myBox queryFoldFunction initialFoldState

        topBeforeAdjustment =
            if minAltitude |> Quantity.greaterThanOrEqualTo Quantity.positiveInfinity then
                baseElevation

            else
                minAltitude

        top =
            -- Just avoid interference with road surface.
            topBeforeAdjustment |> Quantity.minus (meters 0.1)

        myExtrema =
            BoundingBox2d.extrema myBox

        parentExtrema =
            BoundingBox2d.extrema enclosingBox

        contentBox =
            -- This box encloses our points. It defines the top of the frustrum and the base for the next level.
            resultBox
                |> BoundingBox2d.intersection myBox
                |> Maybe.withDefault (BoundingBox2d.singleton centre)

        contentExtrema =
            BoundingBox2d.extrema contentBox

        { nwChildBox, neChildBox, swChildBox, seChildBox } =
            { nwChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.minX myExtrema.maxY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , neChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.maxX myExtrema.maxY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , swChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.minX myExtrema.minY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , seChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.maxX myExtrema.minY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            }

        isNotTiny bx =
            let
                ( width, height ) =
                    BoundingBox2d.dimensions bx

                splitSize =
                    meters <| toFloat options.terrainFineness
            in
            (width |> Quantity.greaterThan splitSize)
                && (height |> Quantity.greaterThan splitSize)
                && count
                > 1

        topColour =
            terrainColourFromHeight <| inMeters top

        sideColour =
            terrainColourFromHeight <| 0.5 * (inMeters top + inMeters baseElevation)

        northernBottomEdge =
            if orientation == NW || orientation == NE then
                parentExtrema.maxY

            else
                myExtrema.maxY

        southernBottomEdge =
            if orientation == SW || orientation == SE then
                parentExtrema.minY

            else
                myExtrema.minY

        easternBottomEdge =
            if orientation == NE || orientation == SE then
                parentExtrema.maxX

            else
                myExtrema.maxX

        westernBottomEdge =
            if orientation == NW || orientation == SW then
                parentExtrema.minX

            else
                myExtrema.minX

        northernSlope =
            -- Better to write this slowly. Use inner minX, maxX at top
            Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz contentExtrema.minX contentExtrema.maxY top)
                (Point3d.xyz contentExtrema.maxX contentExtrema.maxY top)
                (Point3d.xyz easternBottomEdge northernBottomEdge baseElevation)
                (Point3d.xyz westernBottomEdge northernBottomEdge baseElevation)

        southernSlope =
            -- Better to write this slowly. Use inner minX, maxX at top
            Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz contentExtrema.minX contentExtrema.minY top)
                (Point3d.xyz contentExtrema.maxX contentExtrema.minY top)
                (Point3d.xyz easternBottomEdge southernBottomEdge baseElevation)
                (Point3d.xyz westernBottomEdge southernBottomEdge baseElevation)

        westernSlope =
            -- Better to write this slowly. Use inner minX, maxX at top
            Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz contentExtrema.minX contentExtrema.minY top)
                (Point3d.xyz contentExtrema.minX contentExtrema.maxY top)
                (Point3d.xyz westernBottomEdge northernBottomEdge baseElevation)
                (Point3d.xyz westernBottomEdge southernBottomEdge baseElevation)

        easternSlope =
            -- Better to write this slowly. Use inner minX, maxX at top
            Scene3d.quad (Material.matte sideColour)
                (Point3d.xyz contentExtrema.maxX contentExtrema.minY top)
                (Point3d.xyz contentExtrema.maxX contentExtrema.maxY top)
                (Point3d.xyz easternBottomEdge northernBottomEdge baseElevation)
                (Point3d.xyz easternBottomEdge southernBottomEdge baseElevation)

        thisLevelSceneElements =
            if top |> Quantity.greaterThan baseElevation then
                [ Scene3d.quad (Material.matte topColour)
                    (Point3d.xyz contentExtrema.maxX contentExtrema.maxY top)
                    (Point3d.xyz contentExtrema.maxX contentExtrema.minY top)
                    (Point3d.xyz contentExtrema.minX contentExtrema.minY top)
                    (Point3d.xyz contentExtrema.minX contentExtrema.maxY top)
                , northernSlope
                , southernSlope
                , westernSlope
                , easternSlope
                ]

            else
                []
    in
    thisLevelSceneElements
        ++ -- No point recursing if one element only.
           (if isNotTiny myBox then
                List.concat
                    [ terrainFromIndex nwChildBox contentBox NW options top index
                    , terrainFromIndex neChildBox contentBox NE options top index
                    , terrainFromIndex seChildBox contentBox SE options top index
                    , terrainFromIndex swChildBox contentBox SW options top index
                    ]

            else
                []
           )



-- END
