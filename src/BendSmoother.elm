module BendSmoother exposing (..)

import Angle
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Geometry101 as G exposing (..)
import Length exposing (Meters, inMeters, meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import Polyline3d
import PostUpdateActions
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector2d
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Bend smoother classic

Carried over directly from v1, this tool tries to replace
a section of the route between markers with a circular arc.

This will generally give a reasonable curve. It will also
provide a uniform gradient between the markers, which may
not be what you want.

A yellow line shows what the curve will look like, assuming
the algorithm can find one, and this control tab is open.

Use the spacing to vary the number of track points used to
define the bend, and hence the smoothness.

### New in V2

When applied to a single point, the new _Bend Smoother 3D_ will
reduce the gradient and direction changes at the point to below
a set threshold while preserving the gradients before and after."""


type Msg
    = SmoothBend
    | SetBendTrackPointSpacing Float
    | SoftenBend


type alias BendOptions =
    { bendTrackPointSpacing : Float
    , smoothedBend : Maybe SmoothedBend
    }


defaultOptions =
    { bendTrackPointSpacing = 5.0
    , smoothedBend = Nothing
    }


type alias SmoothedBend =
    { nodes : List TrackPoint
    , centre : Point2d Length.Meters LocalCoords
    , radius : Float
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }


type alias DrawingRoad =
    { startsAt : Point3d Meters LocalCoords
    , endsAt : Point3d Meters LocalCoords
    , index : Int
    }


update :
    Msg
    -> BendOptions
    -> Track
    -> ( BendOptions, PostUpdateActions.PostUpdateAction msg )
update msg settings track =
    case msg of
        SetBendTrackPointSpacing spacing ->
            let
                newSettings =
                    { settings | bendTrackPointSpacing = spacing }
            in
            ( newSettings
            , PostUpdateActions.ActionPreview
            )

        SmoothBend ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                (smoothBend track settings)
                (makeUndoMessage settings track)
            )

        SoftenBend ->
            ( settings
            , softenCurrentPoint track
            )


smoothBend : Track -> BendOptions -> Track
smoothBend track options =
    -- The replacement bend is a pre-computed list of Point3d,
    -- We splice them in as Trackpoints.
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode
    in
    case options.smoothedBend of
        Just bend ->
            let
                numCurrentPoints =
                    abs (track.currentNode.index - marker.index) - 1

                numNewPoints =
                    List.length bend.nodes - 2

                newCurrent =
                    if track.currentNode.index > bend.startIndex then
                        List.Extra.getAt
                            (track.currentNode.index - numCurrentPoints + numNewPoints)
                            newTrackPoints
                            |> Maybe.withDefault track.currentNode

                    else
                        track.currentNode

                newMark =
                    if marker.index > bend.startIndex then
                        List.Extra.getAt
                            (marker.index - numCurrentPoints + numNewPoints)
                            newTrackPoints
                            |> Maybe.withDefault marker

                    else
                        marker

                newTrackPoints =
                    List.take bend.startIndex track.track
                        ++ bend.nodes
                        ++ List.drop (bend.endIndex + 1) track.track
            in
            { track
                | track = newTrackPoints
                , currentNode = newCurrent
                , markedNode = Just newMark
            }

        _ ->
            track


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


viewBendFixerPane : BendOptions -> (Msg -> msg) -> Element msg
viewBendFixerPane bendOptions wrap =
    let
        fixBendButton smooth =
            button
                prettyButtonStyles
                { onPress = Just <| wrap SmoothBend
                , label =
                    text <|
                        "Smooth between markers\nRadius "
                            ++ showDecimal2 smooth.radius
                }

        softenButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrap SoftenBend
                , label = text "Smooth current point in 3D"
                }
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ case bendOptions.smoothedBend of
            Just smooth ->
                row [ spacing 10, padding 10, alignTop ]
                    [ fixBendButton smooth
                    , bendSmoothnessSlider bendOptions wrap
                    ]

            Nothing ->
                column [ spacing 10, padding 10, alignTop, centerX ]
                    [ text "Sorry, failed to find a nice bend."
                    , text "Try re-positioning the current pointer or marker."
                    ]
        , softenButton
        ]


bendSmoothnessSlider : BendOptions -> (Msg -> msg) -> Element msg
bendSmoothnessSlider model wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    "Spacing = "
                        ++ showDecimal2 model.bendTrackPointSpacing
        , min = 1.0
        , max = 10.0
        , step = Nothing
        , value = model.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }


softenCurrentPoint : Track -> PostUpdateActions.PostUpdateAction cmd
softenCurrentPoint track =
    PostUpdateActions.ActionTrackChanged
        PostUpdateActions.EditPreservesNodePosition
        (softenSinglePoint track track.currentNode)
        "Smooth single point"


softenSinglePoint : Track -> TrackPoint -> Track
softenSinglePoint track point =
    -- Apply the new bend smoother to a single point, if possible.
    case singlePoint3dArc track point of
        Just arc ->
            let
                precedingTrack =
                    List.take point.index track.track

                remainingTrack =
                    List.drop (point.index + 1) track.track

                newPoints =
                    Arc3d.startPoint arc
                        :: (Arc3d.segments 5 arc
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                           )

                newTrackPoints =
                    List.map trackPointFromPoint newPoints
            in
            { track | track = precedingTrack ++ newTrackPoints ++ remainingTrack }

        Nothing ->
            track


singlePoint3dArc : Track -> TrackPoint -> Maybe (Arc3d Meters LocalCoords)
singlePoint3dArc track point =
    let
        ( a, b, c ) =
            ( List.Extra.getAt (point.index - 1) track.track
            , List.Extra.getAt (point.index + 0) track.track
            , List.Extra.getAt (point.index + 1) track.track
            )
    in
    case ( a, b, c ) of
        ( Just pa, Just pb, Just pc ) ->
            -- Must have three points to play with!
            let
                ( splitAB, splitBC ) =
                    -- Where do we want to begin and end?
                    ( Point3d.interpolateFrom pa.xyz pb.xyz 0.7
                    , Point3d.interpolateFrom pb.xyz pc.xyz 0.3
                    )

                ( abDistFromCorner, bcDistFromCorner ) =
                    -- Which is closest to the corner and hence limits the choice?
                    ( Point3d.distanceFrom splitAB pb.xyz
                    , Point3d.distanceFrom splitBC pb.xyz
                    )

                ( arcStart, arcEnd ) =
                    -- Use shortest length from both edges.
                    if abDistFromCorner |> Quantity.lessThanOrEqualTo bcDistFromCorner then
                        ( splitAB
                        , pb.xyz
                            |> Point3d.translateBy
                                (Vector3d.scaleTo abDistFromCorner pb.roadVector)
                        )

                    else
                        ( pa.xyz
                            |> Point3d.translateBy
                                (Vector3d.scaleTo
                                    (Vector3d.length pa.roadVector |> Quantity.minus bcDistFromCorner)
                                    pa.roadVector
                                )
                        , splitBC
                        )

                trianglePlane =
                    SketchPlane3d.throughPoints pa.xyz pb.xyz pc.xyz
            in
            case trianglePlane of
                -- Points necessarily co-planar but type requires us to check!
                Just plane ->
                    let
                        ( planarA, planarB, planarC ) =
                            -- I think if we project into 2d, the classic logic will hold.
                            ( arcStart |> Point3d.projectInto plane
                            , pb.xyz |> Point3d.projectInto plane
                            , arcEnd |> Point3d.projectInto plane
                            )

                        ( r1Equation, r2Equation ) =
                            ( lineEquationFromTwoPoints
                                (Point2d.toRecord inMeters planarA)
                                (Point2d.toRecord inMeters planarB)
                            , lineEquationFromTwoPoints
                                (Point2d.toRecord inMeters planarB)
                                (Point2d.toRecord inMeters planarC)
                            )

                        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
                            ( linePerpendicularTo r1Equation (Point2d.toRecord inMeters planarA)
                            , linePerpendicularTo r2Equation (Point2d.toRecord inMeters planarC)
                            )

                        circleCenter =
                            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

                        findArc centre =
                            let
                                radius =
                                    distance centre (Point2d.toRecord inMeters planarA)

                                bisector =
                                    Vector2d.from
                                        (Point2d.fromRecord meters centre)
                                        planarB

                                midArcPoint =
                                    Point2d.fromRecord meters centre
                                        |> Point2d.translateBy
                                            (Vector2d.scaleTo (meters radius) bisector)

                                midPoint3d =
                                    midArcPoint |> Point3d.on plane
                            in
                            Arc3d.throughPoints arcStart midPoint3d arcEnd
                    in
                    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter

                Nothing ->
                    Nothing

        _ ->
            Nothing
