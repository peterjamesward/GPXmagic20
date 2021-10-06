module GeometryShared exposing (..)

import Arc3d exposing (Arc3d)
import Geometry101 exposing (distance, lineEquationFromTwoPoints, lineIntersection, linePerpendicularTo)
import Length exposing (Meters, inMeters, meters)
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import Quantity
import SketchPlane3d
import TrackPoint exposing (TrackPoint)
import Vector2d
import Vector3d


arc3dFromThreePoints : TrackPoint -> TrackPoint -> TrackPoint -> Maybe (Arc3d Meters LocalCoords)
arc3dFromThreePoints pa pb pc =
    -- Must have three points to play with!
    let
        (beforeLength, afterLength) =
            (Point3d.distanceFrom pa.xyz pb.xyz, Point3d.distanceFrom pb.xyz pc.xyz)

        amountToStealFromFirstSegment =
            Quantity.min (meters 4.0) (Quantity.divideBy 2.0 beforeLength)

        amountToStealFromSecondSegment =
            Quantity.min (meters 4.0) (Quantity.divideBy 2.0 afterLength)

        commonAmountToSteal =
            Quantity.min amountToStealFromFirstSegment amountToStealFromSecondSegment

        arcStart =
            Point3d.interpolateFrom
                pb.xyz
                pa.xyz
                (Quantity.ratio commonAmountToSteal beforeLength)

        arcEnd =
            Point3d.interpolateFrom
                pb.xyz
                pc.xyz
                (Quantity.ratio commonAmountToSteal afterLength)

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
