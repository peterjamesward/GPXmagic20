module RoadIndex exposing (Intersection, intersections)

{-
   Uses SpatialIndex to build a quickly queryable index of road segments.
-}

import BoundingBox2d
import Length exposing (Meters, meters)
import LineSegment2d exposing (endPoint, startPoint)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (distanceFrom)
import Point3d
import Quantity
import SketchPlane3d
import SpatialIndex exposing (SpatialContent)
import TrackPoint exposing (TrackPoint)


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


type alias Segment =
    -- The Int is what allows us to refer back to source Trackpoints
    { id : TrackPoint, line : LineSegment2d.LineSegment2d Length.Meters LocalCoords }


type alias Index =
    SpatialIndex.SpatialNode Segment Length.Meters LocalCoords


type alias Intersection =
    { segments : ( Segment, Segment )
    , intersectAt : Point2d.Point2d Length.Meters LocalCoords
    }


intersections : List TrackPoint -> List Intersection
intersections trackpoints =
    -- Top level API
    let
        makeSegment tp1 tp2 =
            { id = tp1
            , line =
                LineSegment2d.from
                    (tp1.xyz |> Point3d.projectInto SketchPlane3d.xy)
                    (tp2.xyz |> Point3d.projectInto SketchPlane3d.xy)
            }

        segments =
            List.map2 makeSegment
                trackpoints
                (List.drop 1 trackpoints)
    in
    case
        trackpoints
            |> BoundingBox2d.hullOfN (.xyz >> Point3d.projectInto SketchPlane3d.xy)
    of
        Just box ->
            let
                index =
                    SpatialIndex.empty box (Length.meters 5.0)

                initialState : ( Index, List Intersection )
                initialState =
                    ( index, [] )

                finalState : ( Index, List Intersection )
                finalState =
                    List.foldl checkSegmentAndAdd initialState segments
            in
            Tuple.second finalState

        Nothing ->
            []


checkSegmentAndAdd : Segment -> ( Index, List Intersection ) -> ( Index, List Intersection )
checkSegmentAndAdd segment ( index, intersects ) =
    -- This is called for each segment.
    -- We search for overlaps.
    -- We look at these overlaps to see if any are real intersections.
    -- We add intersections to the list.
    -- We add this segment to the index.
    -- TODO: Ignore colinear segments, e.g. on out and back sections. \
    -- Well, the intersection test already ignores colinear, so I think this is a tolerance thing
    -- and we'd be safer rejecting sufficiently small triangles.
    let
        prepContent =
            { content = segment
            , box = LineSegment2d.boundingBox segment.line
            }

        overlaps : List Segment
        overlaps =
            SpatialIndex.query index prepContent.box
                |> List.map .content

        intersectingLines : List Intersection
        intersectingLines =
            overlaps
                |> List.filterMap checkForIntersect

        samePoint p1 p2 =
            p1 |> distanceFrom p2 |> Quantity.lessThan (meters 0.1)

        checkForIntersect : Segment -> Maybe Intersection
        checkForIntersect overlap =
            case LineSegment2d.intersectionPoint overlap.line segment.line of
                Just crossing ->
                    -- Note that adjacent segment must not be counted!
                    --TODO: We really need the TP Index here to see if they are
                    -- adjacent on the route rather than just by chance.
                    if
                        samePoint (startPoint overlap.line) (endPoint segment.line)
                            || samePoint (startPoint segment.line) (endPoint overlap.line)
                            --|| samePoint (startPoint segment.line) (startPoint overlap.line)
                            --|| samePoint (endPoint segment.line) (endPoint overlap.line)
                    then
                        Nothing

                    else
                        Just
                            { segments = ( segment, overlap )
                            , intersectAt = crossing
                            }

                Nothing ->
                    Nothing
    in
    ( SpatialIndex.add prepContent index
    , intersectingLines ++ intersects
    )
