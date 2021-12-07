module TrackSearchQueries exposing (..)

import Axis2d
import Axis3d exposing (Axis3d)
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point2d
import Point3d
import SketchPlane3d
import SpatialIndex
import TrackPoint exposing (TrackPoint)


trackPointNearestFromIndexFor3d :
    SpatialIndex.SpatialNode TrackPoint Length.Meters LocalCoords
    -> Axis3d Meters LocalCoords
    -> Maybe TrackPoint
trackPointNearestFromIndexFor3d index ray =
    let
        rayShadow =
            ray
                |> Axis3d.projectInto SketchPlane3d.xy
                |> Maybe.withDefault Axis2d.x

        distanceFunction =
            .xyz
                >> Point3d.distanceFromAxis ray
                >> Length.inMeters
    in
    SpatialIndex.queryNearestToAxisUsing index rayShadow distanceFunction
        |> Maybe.map .content


trackPointNearestFromIndexForPlan :
    SpatialIndex.SpatialNode TrackPoint Length.Meters LocalCoords
    -> Axis3d Meters LocalCoords
    -> Maybe TrackPoint
trackPointNearestFromIndexForPlan index ray =
    let
        groundZero =
            ray
                |> Axis3d.intersectionWithPlane Plane3d.xy
                |> Maybe.withDefault Point3d.origin
                |> Point3d.projectInto SketchPlane3d.xy

        nearbyPoints =
            SpatialIndex.queryAllContaining index groundZero
    in
    List.Extra.minimumBy
        (.content
            >> .xyz
            >> Point3d.projectInto SketchPlane3d.xy
            >> Point2d.distanceFrom groundZero
            >> Length.inMeters
        )
        nearbyPoints
        |> Maybe.map .content
