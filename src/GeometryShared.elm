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
