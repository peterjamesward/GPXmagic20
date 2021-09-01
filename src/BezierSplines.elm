module BezierSplines exposing (..)

import CubicSpline3d exposing (CubicSpline3d)
import Length
import LineSegment3d exposing (LineSegment3d)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Triangle3d exposing (Triangle3d)
import Vector3d


type alias ControlPoint =
    Point3d Length.Meters LocalCoords


bezierSplines : Bool -> Float -> Float -> List TrackPoint -> List TrackPoint
bezierSplines isLoop tension tolerance trackPoints =
    let
        points =
            -- Shim for v1 code.
            List.map .xyz trackPoints

        firstPoint =
            -- This is used for wrap-around on loop, in which case use second point
            if isLoop then
                List.take 1 <| List.drop 1 points

            else
                List.take 1 points

        lastPoint =
            -- This is used for wrap-around on loop, in which case use penultimate point
            if isLoop then
                List.take 1 <| List.drop 1 <| List.reverse points

            else
                List.take 1 <| List.reverse points

        makeTriangles : List (Triangle3d Length.Meters LocalCoords)
        makeTriangles =
            let
                shiftedBack =
                    if isLoop then
                        lastPoint ++ points

                    else
                        firstPoint ++ points

                shiftedForwards =
                    if isLoop then
                        List.drop 1 points ++ firstPoint

                    else
                        List.drop 1 points ++ lastPoint
            in
            List.map3
                Triangle3d.from
                shiftedBack
                points
                shiftedForwards

        controlPointsFromTriangle :
            Triangle3d Length.Meters LocalCoords
            -> ( ControlPoint, ControlPoint, ControlPoint )
        controlPointsFromTriangle triangle =
            let
                ( _, b, _ ) =
                    Triangle3d.vertices triangle

                ( entryEdge, oppositeEdge, exitEdge ) =
                    Triangle3d.edges triangle

                ( ab, ac, bc ) =
                    ( Length.inMeters <| LineSegment3d.length entryEdge
                    , Length.inMeters <| LineSegment3d.length oppositeEdge
                    , Length.inMeters <| LineSegment3d.length exitEdge
                    )

                ( entryFactor, exitFactor ) =
                    ( -1.0 * tension * ab / (ab + bc)
                    , tension * bc / (ab + bc)
                    )

                controlPointVector =
                    Vector3d.from
                        (LineSegment3d.startPoint oppositeEdge)
                        (LineSegment3d.endPoint oppositeEdge)

                ( entryScaleVector, exitScalevector ) =
                    ( Vector3d.scaleBy entryFactor controlPointVector
                    , Vector3d.scaleBy exitFactor controlPointVector
                    )

                ( entryPoint, exitPoint ) =
                    ( Point3d.translateBy entryScaleVector b
                    , Point3d.translateBy exitScalevector b
                    )
            in
            ( entryPoint, b, exitPoint )

        makeControlPoints : List ( ControlPoint, ControlPoint, ControlPoint )
        makeControlPoints =
            List.map
                controlPointsFromTriangle
                makeTriangles

        makeSpline :
            ( ControlPoint, ControlPoint, ControlPoint )
            -> ( ControlPoint, ControlPoint, ControlPoint )
            -> CubicSpline3d Length.Meters LocalCoords
        makeSpline ( _, start, control1 ) ( control2, end, _ ) =
            CubicSpline3d.fromControlPoints
                start
                control1
                control2
                end

        makeSplines : List (CubicSpline3d Length.Meters LocalCoords)
        makeSplines =
            List.map2
                makeSpline
                makeControlPoints
                (List.drop 1 makeControlPoints)

        asPolylines : List (Polyline3d Length.Meters LocalCoords)
        asPolylines =
            List.map
                (CubicSpline3d.approximate (Length.meters tolerance))
                makeSplines

        asSegments : List (LineSegment3d Length.Meters LocalCoords)
        asSegments =
            List.concatMap
                Polyline3d.segments
                asPolylines

        asPointsAgain : List ControlPoint
        asPointsAgain =
            List.map
                LineSegment3d.startPoint
                (List.take 1 asSegments)
                ++ List.map
                    LineSegment3d.endPoint
                    asSegments
    in
    List.map trackPointFromPoint asPointsAgain


bezierApproximation : Bool -> Float -> Float -> List TrackPoint -> List TrackPoint
bezierApproximation _ _ tolerance points =
    -- This variant uses existing points as controls, and lets the result approximate the route.
    -- Arguments compatible; loopiness and tension not used.
    let
        rawPoints =
            List.map .xyz points

        makeSpline first second third =
            let
                ( start, end ) =
                    ( Point3d.midpoint first second
                    , Point3d.midpoint second third
                    )
            in
            CubicSpline3d.fromControlPoints
                start
                second
                second
                end

        makeSplines =
            List.map3
                makeSpline
                rawPoints
                (List.drop 1 rawPoints)
                (List.drop 2 rawPoints)

        asPolylines =
            List.map
                (CubicSpline3d.approximate (Length.meters tolerance))
                makeSplines

        asSegments =
            List.concatMap
                Polyline3d.segments
                asPolylines

        asPointsAgain =
            List.map
                LineSegment3d.startPoint
                (List.take 1 asSegments)
                ++ List.map
                    LineSegment3d.endPoint
                    asSegments
    in
    List.take 1 points
        ++ List.map trackPointFromPoint asPointsAgain
        ++ List.drop (List.length points - 1) points



-- END
