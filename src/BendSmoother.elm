module BendSmoother exposing (..)

import Angle
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Color
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Geometry101 as G exposing (..)
import GeometryShared exposing (arc3dFromThreePoints)
import Json.Encode as E
import Length exposing (Meters, inMeters, meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import Polyline3d
import PostUpdateActions exposing (EditResult, UndoEntry, defaultEditResult)
import Scene3d exposing (Entity)
import SceneBuilder exposing (previewLine)
import SceneBuilderProfile exposing (previewProfileLine)
import Track exposing (Track)
import TrackEditType
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Utils exposing (showDecimal0, showShortMeasure)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


toolLabel =
    "Bend smoother classic"


info =
    """## Bend smoother classic

This tool tries to replace
a section of the route between markers with a circular arc.

This will generally give a reasonable curve. It will also
provide a uniform gradient between the markers, which may
not be what you want.

A yellow line shows what the curve will look like, assuming
the algorithm can find one, and this control tab is open.

Use the slider to vary the spacing between track points used to
define the bend, and hence the smoothness.

When applied to a single point, _Smooth current point_ will
reduce the gradient and direction changes at the point by replacing
one trackpoint with two or more along an arc. The number added
is determined by the _Segments_ setting."""


type Msg
    = SmoothBend
    | SetBendTrackPointSpacing Float
    | SetSegments Int
    | SmoothPoint


type alias BendOptions =
    { bendTrackPointSpacing : Float
    , segments : Int
    , smoothBend : Maybe SmoothedBend
    }


defaultOptions =
    { bendTrackPointSpacing = 5.0
    , segments = 1
    , smoothBend = Nothing
    }


type alias SmoothedBend =
    -- This becomes the Undo/Redo info.
    { nodes : List TrackPoint
    , centre : Point2d Length.Meters LocalCoords
    , radius : Float
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }


type alias UndoRedoInfo =
    { newBend : SmoothedBend
    , oldNodes : List TrackPoint
    }


type alias DrawingRoad =
    -- Helps with interacting with my v1 geometry routines.
    { startsAt : Point3d Meters LocalCoords
    , endsAt : Point3d Meters LocalCoords
    , index : Int
    }


tryBendSmoother : Track -> BendOptions -> Maybe SmoothedBend
tryBendSmoother track options =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.currentNode.index <= marker.index then
                ( track.currentNode, marker )

            else
                ( marker, track.currentNode )
    in
    if endPoint.index >= startPoint.index + 2 then
        lookForSmoothBendOption options.bendTrackPointSpacing track startPoint endPoint

    else
        Nothing


buildSmoothBendActions : BendOptions -> Track -> UndoEntry
buildSmoothBendActions options track =
    -- This is the +/-ve delta for possible redo. We do not include track in the closure!
    case tryBendSmoother track options of
        Just bend ->
            let
                undoRedoInfo =
                    { newBend = bend
                    , oldNodes =
                        track.trackPoints
                            |> List.drop bend.startIndex
                            |> List.take (bend.endIndex - bend.startIndex + 1)
                    }
            in
            { label = makeUndoMessage options track
            , editFunction = applySmoothBend undoRedoInfo
            , undoFunction = undoSmoothBend undoRedoInfo
            , newOrange = track.currentNode.index
            , newPurple = Maybe.map .index track.markedNode
            , oldOrange = track.currentNode.index
            , oldPurple = Maybe.map .index track.markedNode
            }

        Nothing ->
            { label = "Something amiss"
            , editFunction = always defaultEditResult
            , undoFunction = always defaultEditResult
            , newOrange = track.currentNode.index
            , newPurple = Maybe.map .index track.markedNode
            , oldOrange = track.currentNode.index
            , oldPurple = Maybe.map .index track.markedNode
            }


update :
    Msg
    -> BendOptions
    -> Track
    -> ( BendOptions, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        SetBendTrackPointSpacing spacing ->
            let
                newSettings =
                    { settings | bendTrackPointSpacing = spacing }
            in
            ( { newSettings | smoothBend = tryBendSmoother track newSettings }
            , PostUpdateActions.ActionPreview
            )

        SetSegments segments ->
            let
                newSettings =
                    { settings | segments = segments }
            in
            ( { newSettings | smoothBend = tryBendSmoother track newSettings }
            , PostUpdateActions.ActionPreview
            )

        SmoothBend ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildSmoothBendActions settings track)
            )

        SmoothPoint ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildSmoothPointActions settings track)
            )


applySmoothBend : UndoRedoInfo -> Track -> EditResult
applySmoothBend undoRedoInfo track =
    -- The replacement bend is a pre-computed list of Point3d,
    -- We splice them in as Trackpoints.
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.newBend.startIndex

        ( _, suffix ) =
            theRest |> List.Extra.splitAt (undoRedoInfo.newBend.endIndex - undoRedoInfo.newBend.startIndex)
    in
    { before = prefix
    , edited = undoRedoInfo.newBend.nodes
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


undoSmoothBend : UndoRedoInfo -> Track -> EditResult
undoSmoothBend undoRedoInfo track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.newBend.startIndex

        ( _, suffix ) =
            theRest |> List.Extra.splitAt (List.length undoRedoInfo.newBend.nodes)
    in
    { before = prefix
    , edited = undoRedoInfo.oldNodes
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


makeUndoMessage : BendOptions -> Track -> String
makeUndoMessage options track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( dist1, dist2 ) =
            ( Length.inMeters track.currentNode.distanceFromStart
            , Length.inMeters markerPosition.distanceFromStart
            )

        ( from, to ) =
            ( min dist1 dist2
            , max dist1 dist2
            )
    in
    "Smooth bend " ++ showDecimal0 from ++ " to " ++ showDecimal0 to


roadToGeometry : DrawingRoad -> G.Road
roadToGeometry road =
    { startAt =
        { x = Length.inMeters <| xCoordinate road.startsAt
        , y = Length.inMeters <| yCoordinate road.startsAt
        }
    , endsAt =
        { x = Length.inMeters <| xCoordinate road.endsAt
        , y = Length.inMeters <| yCoordinate road.endsAt
        }
    }


lookForSmoothBendOption :
    Float
    -> Track
    -> TrackPoint
    -> TrackPoint
    -> Maybe SmoothedBend
lookForSmoothBendOption trackPointSpacing track pointA pointD =
    let
        ( roadAB, roadCD ) =
            -- Try to make minimal changes from v1. Is that wise?
            ( { startsAt = pointA.xyz
              , endsAt = .xyz <| Track.nextPointOn track pointA
              , index = pointA.index
              }
            , { startsAt = .xyz <| Track.prevPointOn track pointD
              , endsAt = pointD.xyz
              , index = pointD.index
              }
            )

        ( roadIn, roadOut ) =
            ( roadToGeometry roadAB, roadToGeometry roadCD )

        arcFinderGeneral p =
            if isBefore roadIn p && isAfter roadOut p then
                divergentRoadsArc p roadIn roadOut

            else if isAfter roadIn p && isBefore roadOut p then
                convergentRoadsArc p roadIn roadOut

            else
                Nothing

        arc =
            case findIntercept roadIn roadOut of
                Nothing ->
                    parallelFindSemicircle roadIn roadOut

                Just p ->
                    arcFinderGeneral p
    in
    Maybe.map (makeSmoothBend trackPointSpacing roadAB roadCD) arc


withoutElevation : Point3d Length.Meters LocalCoords -> Point2d Length.Meters LocalCoords
withoutElevation p3 =
    let
        { x, y, z } =
            Point3d.toMeters p3
    in
    Point2d.fromMeters { x = x, y = y }


withElevation : Float -> Point2d Length.Meters LocalCoords -> Point3d Length.Meters LocalCoords
withElevation elevation p2 =
    let
        { x, y } =
            Point2d.toMeters p2
    in
    Point3d.fromMeters { x = x, y = y, z = elevation }


makeSmoothBend :
    Float
    -> DrawingRoad
    -> DrawingRoad
    -> Arc2d Meters LocalCoords
    -> SmoothedBend
makeSmoothBend trackPointSpacing roadAB roadCD arc =
    -- Note return list here includes points A and D.
    let
        trueArcLength =
            (abs <| Angle.inRadians <| Arc2d.sweptAngle arc)
                * (Length.inMeters <| Arc2d.radius arc)

        numberPointsOnArc =
            ceiling <| trueArcLength / trackPointSpacing

        segments =
            Arc2d.segments (numberPointsOnArc - 1) arc
                |> Polyline2d.segments

        realArcLength =
            List.sum <| List.map (inMeters << LineSegment2d.length) segments

        ( p1, p2 ) =
            -- The first (last) tangent point is also the first (last) point on the arc
            -- so we don't need to pass these as arguments.
            ( Arc2d.startPoint arc |> Point2d.toRecord inMeters
            , Arc2d.endPoint arc |> Point2d.toRecord inMeters
            )

        ( elevationAtA, elevationAtD ) =
            ( Length.inMeters <| zCoordinate roadAB.startsAt
            , Length.inMeters <| zCoordinate roadCD.endsAt
            )

        ( tang1, tang2 ) =
            -- Say the arc entry is at same elevation as end points
            ( Point3d.fromTuple Length.meters ( p1.x, p1.y, elevationAtA )
            , Point3d.fromTuple Length.meters ( p2.x, p2.y, elevationAtD )
            )

        ( entryStraightLength, exitStraightLength ) =
            ( Length.inMeters <| Point3d.distanceFrom roadAB.startsAt tang1
            , Length.inMeters <| Point3d.distanceFrom tang2 roadCD.endsAt
            )

        totalNewLength =
            -- if we ignore gradient for now
            entryStraightLength + realArcLength + exitStraightLength

        ( tangent1Elevation, tangent2Elevation ) =
            ( elevationAtA + (entryStraightLength / totalNewLength) * (elevationAtD - elevationAtA)
            , elevationAtD + (exitStraightLength / totalNewLength) * (elevationAtA - elevationAtD)
            )

        ( newEntryPoint, newExitPoint ) =
            ( Point3d.translateBy
                (Vector3d.fromMeters { x = 0, y = 0, z = tangent1Elevation - elevationAtA })
                tang1
            , Point3d.translateBy
                (Vector3d.fromMeters { x = 0, y = 0, z = tangent2Elevation - elevationAtD })
                tang2
            )

        eleIncrement =
            (tangent2Elevation - tangent1Elevation) / (toFloat numberPointsOnArc - 1)

        elevate point2d i =
            withElevation
                (tangent1Elevation + toFloat i * eleIncrement)
                point2d

        newArcPoints =
            List.map2
                elevate
                (List.map LineSegment2d.startPoint <| List.drop 1 segments)
                (List.range 1 (numberPointsOnArc - 1))

        asTrackPoints =
            [ roadAB.startsAt, newEntryPoint ]
                ++ newArcPoints
                ++ [ newExitPoint, roadCD.endsAt ]
                |> List.map trackPointFromPoint
    in
    { nodes = asTrackPoints
    , centre = Arc2d.centerPoint arc
    , radius = inMeters <| Arc2d.radius arc
    , startIndex = roadAB.index
    , endIndex = roadCD.index
    }


divergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
divergentRoadsArc p r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt ), ( r2.startAt, r2.endsAt ) )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            -- For divergence, choose midpoint farthest from interesct p.
            if distance p midAB >= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pc) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pb) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = p, endsAt = centre }

                midArcPoint =
                    pointAlongRoad
                        bisectorAsRoad
                        (radius + distance p centre)
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


convergentRoadsArc : Point -> Road -> Road -> Maybe (Arc2d Meters LocalCoords)
convergentRoadsArc p r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt )
            , ( r2.startAt, r2.endsAt )
            )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            if distance p midAB <= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pd) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pa) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = centre, endsAt = p }

                midArcPoint =
                    pointAlongRoad bisectorAsRoad radius
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


parallelFindSemicircle : Road -> Road -> Maybe (Arc2d Meters LocalCoords)
parallelFindSemicircle r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( r1.startAt, r1.endsAt )
            , ( r2.startAt, r2.endsAt )
            )

        ( midAB, midBC ) =
            ( interpolateLine 0.5 pa pb
            , interpolateLine 0.5 pb pc
            )

        ( midCD, midDA ) =
            ( interpolateLine 0.5 pc pd
            , interpolateLine 0.5 pd pa
            )

        middle =
            -- As lines are parallel, we can use this as the circle centre.
            interpolateLine 0.5 midBC midDA

        centreLine =
            { startAt = middle, endsAt = midBC }

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( radiusToFirstTangentPoint, radiusToSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation middle, linePerpendicularTo r2Equation middle )

        ( firstTangentPoint, secondTangentPoint ) =
            ( lineIntersection r1Equation radiusToFirstTangentPoint
            , lineIntersection r2Equation radiusToSecondTangentPoint
            )
    in
    case ( firstTangentPoint, secondTangentPoint ) of
        ( Just t1, Just t2 ) ->
            let
                radius =
                    distance middle t1

                midArcPoint =
                    pointAlongRoad centreLine radius
            in
            Arc2d.throughPoints
                (Point2d.meters t1.x t1.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters t2.x t2.y)

        _ ->
            Nothing


viewBendFixerPane : Bool -> BendOptions -> Maybe Track -> (Msg -> msg) -> Element msg
viewBendFixerPane imperial bendOptions mtrack wrap =
    case mtrack of
        Nothing ->
            none

        Just track ->
            let
                updatedSettings =
                    -- Because, say, someone moved the markers...
                    { bendOptions | smoothBend = tryBendSmoother track bendOptions }

                fixBendButton =
                    button
                        prettyButtonStyles
                    <|
                        case updatedSettings.smoothBend of
                            Just isSmooth ->
                                { onPress = Just <| wrap SmoothBend
                                , label =
                                    text <|
                                        "Smooth bend with\nradius "
                                            ++ showShortMeasure imperial (Length.meters isSmooth.radius)
                                }

                            Nothing ->
                                { onPress = Nothing
                                , label = text "No bend found"
                                }

                softenButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap SmoothPoint
                        , label = text "Smooth current point"
                        }
            in
            wrappedRow [ spacing 10, padding 10 ] <|
                [ bendSmoothnessSlider imperial bendOptions wrap
                , fixBendButton
                , segmentSlider bendOptions wrap
                , softenButton
                ]


bendSmoothnessSlider : Bool -> BendOptions -> (Msg -> msg) -> Element msg
bendSmoothnessSlider imperial model wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    "Spacing: "
                        ++ showShortMeasure imperial (Length.meters model.bendTrackPointSpacing)
        , min =
            Length.inMeters <|
                if imperial then
                    Length.feet 3.0

                else
                    Length.meters 1.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 30.0

                else
                    Length.meters 10.0
        , step = Nothing
        , value = model.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }


segmentSlider : BendOptions -> (Msg -> msg) -> Element msg
segmentSlider model wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetSegments << round
        , label =
            Input.labelBelow [] <|
                text <|
                    "Segments: "
                        ++ String.fromInt model.segments
        , min = 1.0
        , max = 10.0
        , step = Just 1.0
        , value = toFloat model.segments
        , thumb = Input.defaultThumb
        }


type alias SinglePointUndoRedo =
    { numberOfSegments : Int
    , indices : List Int
    , xyzs : List (Point3d.Point3d Length.Meters LocalCoords)
    }


buildSmoothPointActions : BendOptions -> Track -> UndoEntry
buildSmoothPointActions options track =
    -- This is the +/-ve delta for possible redo. We do not include track in the closure!
    let
        undoRedoInfo : SinglePointUndoRedo
        undoRedoInfo =
            { indices = [ track.currentNode.index ]
            , numberOfSegments = options.segments
            , xyzs = [ track.currentNode.xyz ]
            }
    in
    { label = "Smooth point " ++ String.fromInt track.currentNode.index
    , editFunction = applySmoothPoints undoRedoInfo
    , undoFunction = undoSmoothPoints undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


applySmoothPoints : SinglePointUndoRedo -> Track -> EditResult
applySmoothPoints undoRefoInfo track =
    -- Best to do the multiple point case, and the single one is just the trivial case.
    -- The essence of 'apply' is to split the track into fragments at the nominated points,
    -- replace the single points with the smoothed verions, and recombine.
    let
        curveAtPoint point =
            case singlePoint3dArc track point of
                Just arc ->
                    Arc3d.startPoint arc
                        :: (Arc3d.segments undoRefoInfo.numberOfSegments arc
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                           )
                        |> List.map trackPointFromPoint

                Nothing ->
                    -- We know there's an arc, because the button is enabled, but ...
                    []

        ( sectionsUntouched, nodesToSmooth ) =
            track.trackPoints
                |> Utils.subListsByWithSingletons
                    (\tp -> List.member tp.index undoRefoInfo.indices)

        ( prefix, suffix ) =
            -- There may be large sections either side; let's respect that.
            ( List.head sectionsUntouched |> Maybe.withDefault []
            , List.Extra.last sectionsUntouched |> Maybe.withDefault []
            )

        fillers : List (List TrackPoint)
        fillers =
            -- These are what we interweave between the new curved nodes.
            sectionsUntouched
                |> List.take (List.length sectionsUntouched - 1)
                |> List.drop 1

        smoothedNodes : List (List TrackPoint)
        smoothedNodes =
            -- Note that nodesToSmooth is a list of list. We can't
            List.map curveAtPoint nodesToSmooth
    in
    { before = prefix
    , edited = List.Extra.interweave smoothedNodes fillers |> List.concat
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


undoSmoothPoints : SinglePointUndoRedo -> Track -> EditResult
undoSmoothPoints undoRedoInfo track =
    -- Best to do the multiple point case, and the single one is just the trivial case.
    -- The essence of 'undo' is to split the track into fragments at the nominated points,
    -- (accounting for the additional points added by the 'apply')
    -- replace the the smoothed verions with the single points, and recombine.
    -- Similar approach to apply but each point is replaced by (numSegments + 1) points,
    -- so we need to work out the new numbers and hence our splitting predicate.
    -- E.g. if our nodes were [1, 10, 20], and numSegments = 3
    -- [1] is now [1..4] so rest bumped by 3.
    -- [10] is now [13..16], so rest bumped by 6.
    -- [20] is now [26..29].
    let
        indexBumps =
            List.range 0 (List.length undoRedoInfo.indices - 1)
                |> List.map ((*) undoRedoInfo.numberOfSegments)

        bumpedIndices =
            List.map2 (+) undoRedoInfo.indices indexBumps

        ( sectionsUntouched, bendNodes ) =
            Utils.subListsByGreedy
                (\tp -> List.member tp.index bumpedIndices)
                (undoRedoInfo.numberOfSegments + 1)
                track.trackPoints

        ( prefix, suffix ) =
            -- There may be large sections either side; let's respect that.
            ( List.head sectionsUntouched |> Maybe.withDefault []
            , List.Extra.last sectionsUntouched |> Maybe.withDefault []
            )

        fillers : List (List TrackPoint)
        fillers =
            -- These are what we interweave between the new curved nodes.
            sectionsUntouched
                |> List.take (List.length sectionsUntouched - 1)
                |> List.drop 1

        restoredPoints =
            undoRedoInfo.xyzs
                |> List.map (trackPointFromPoint >> List.singleton)
    in
    { before = prefix
    , edited = List.Extra.interweave restoredPoints fillers |> List.concat
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


singlePoint3dArc : Track -> TrackPoint -> Maybe (Arc3d Meters LocalCoords)
singlePoint3dArc track point =
    let
        ( a, b, c ) =
            ( List.Extra.getAt (point.index - 1) track.trackPoints
            , List.Extra.getAt (point.index + 0) track.trackPoints
            , List.Extra.getAt (point.index + 1) track.trackPoints
            )
    in
    case ( a, b, c ) of
        ( Just pa, Just pb, Just pc ) ->
            arc3dFromThreePoints pa pb pc

        _ ->
            Nothing


getPreview3D : BendOptions -> Track -> List (Entity LocalCoords)
getPreview3D options track =
    let
        updatedSettings =
            -- Because, say, someone moved the markers...
            { options | smoothBend = tryBendSmoother track options }

        undoEntry =
            buildSmoothBendActions updatedSettings track

        results =
            undoEntry.editFunction track
    in
    previewLine Color.yellow results.edited


getPreviewProfile : DisplayOptions -> BendOptions -> Track -> List (Entity LocalCoords)
getPreviewProfile display options track =
    let
        updatedSettings =
            -- Because, say, someone moved the markers...
            { options | smoothBend = tryBendSmoother track options }

        undoEntry =
            buildSmoothBendActions updatedSettings track

        results =
            undoEntry.editFunction track
    in
    previewProfileLine display Color.yellow results.edited


getPreviewMap : DisplayOptions -> BendOptions -> Track -> E.Value
getPreviewMap display options track =
    {-
       To return JSON:
       { "name" : "nudge"
       , "colour" : "#FFFFFF"
       , "points" : <trackPointsToJSON ...>
       }
    -}
    let
        updatedSettings =
            -- Because, say, someone moved the markers...
            { options | smoothBend = tryBendSmoother track options }

        undoEntry =
            buildSmoothBendActions updatedSettings track

        results =
            undoEntry.editFunction track

        fakeTrack =
            -- Just for the JSON
            { track | trackPoints = results.edited }
    in
    E.object
        [ ( "name", E.string "bend" )
        , ( "colour", E.string "#FFFF00" )
        , ( "points", Track.trackToJSON fakeTrack )
        ]


multiplePointSmoothing :
    List TrackPoint
    -> Int
    -> Track
    -> PostUpdateActions.PostUpdateAction trck msg
multiplePointSmoothing trackPoints numSegments track =
    -- This is called only from the Track Observations tab.
    -- Tempted to deprecate it, but people.
    let
        undoRedoInfo : SinglePointUndoRedo
        undoRedoInfo =
            { indices = List.map .index trackPoints
            , numberOfSegments = numSegments
            , xyzs = List.map .xyz trackPoints
            }
    in
    PostUpdateActions.ActionTrackChanged
        TrackEditType.EditPreservesIndex
        { label = "Smooth " ++ (String.fromInt <| List.length trackPoints) ++ " points"
        , editFunction = applySmoothPoints undoRedoInfo
        , undoFunction = undoSmoothPoints undoRedoInfo
        , newOrange = track.currentNode.index
        , newPurple = Maybe.map .index track.markedNode
        , oldOrange = track.currentNode.index
        , oldPurple = Maybe.map .index track.markedNode
        }
