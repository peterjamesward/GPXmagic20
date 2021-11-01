module SpatialIndex exposing (..)

{-
   This is a simple quadtree based method for tracking bounding boxes.
   It's only requirement is to detect overlaps, with reasonable efficiency.
-}

import BoundingBox2d
import Point2d
import Quantity exposing (Quantity(..))
import Quantity.Interval as Interval


type alias SpatialContent contentType units coords =
    { content : contentType
    , box : BoundingBox2d.BoundingBox2d units coords
    }


type SpatialNode contentType units coords
    = SpatialNode
        { box : BoundingBox2d.BoundingBox2d units coords
        , minSize : Quantity Float units
        , contents : List (SpatialContent contentType units coords)
        , nw : SpatialNode contentType units coords
        , ne : SpatialNode contentType units coords
        , se : SpatialNode contentType units coords
        , sw : SpatialNode contentType units coords
        }
    | Blank


empty : BoundingBox2d.BoundingBox2d units coords -> Quantity Float units -> SpatialNode contentType units coords
empty box minSize =
    SpatialNode
        { box = box
        , minSize = minSize
        , contents = []
        , nw = Blank
        , ne = Blank
        , se = Blank
        , sw = Blank
        }


add : SpatialContent contentType units coords -> SpatialNode contentType units coords -> SpatialNode contentType units coords
add content current =
    -- If the content will fit into a child, pass it down the child, new child if needed.
    -- Otherwise, add it to the contents at this level.
    case current of
        Blank ->
            -- Oops.
            Blank

        SpatialNode node ->
            let
                { minX, maxX, minY, maxY } =
                    BoundingBox2d.extrema node.box

                centre =
                    BoundingBox2d.centerPoint node.box

                canSplit =
                    let
                        ( xInterval, yInterval ) =
                            BoundingBox2d.intervals node.box
                    in
                    (Interval.width xInterval |> Quantity.greaterThan node.minSize)
                        && (Interval.width yInterval |> Quantity.greaterThan node.minSize)

                { nw, ne, sw, se } =
                    { nw = BoundingBox2d.from centre (Point2d.xy minX maxY)
                    , ne = BoundingBox2d.from centre (Point2d.xy maxX maxY)
                    , sw = BoundingBox2d.from centre (Point2d.xy minX minY)
                    , se = BoundingBox2d.from centre (Point2d.xy maxX minY)
                    }

                addToChild :
                    SpatialNode contentType units coords
                    -> BoundingBox2d.BoundingBox2d units coords
                    -> SpatialNode contentType units coords
                addToChild child box =
                    case child of
                        SpatialNode _ ->
                            add content child

                        Blank ->
                            add content (empty box node.minSize)
            in
            if canSplit && (content.box |> BoundingBox2d.isContainedIn nw) then
                SpatialNode
                    { node
                        | nw = addToChild node.nw nw
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn ne) then
                SpatialNode
                    { node
                        | ne = addToChild node.ne ne
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn sw) then
                SpatialNode
                    { node
                        | sw = addToChild node.sw sw
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn se) then
                SpatialNode
                    { node
                        | se = addToChild node.se se
                    }

            else
                SpatialNode { node | contents = content :: node.contents }


query :
    SpatialNode contentType units coords
    -> SpatialContent contentType units coords
    -> List (SpatialContent contentType units coords)
query current specimen =
    -- We are only expected to return content whose bounding box overlaps
    -- with the bounding box of the specimen. We do this by looking in a relevant child
    -- or in our own list, depending on the extent of the speciment compared to our children.
    let
        contentHasOverlap node =
            BoundingBox2d.intersects node.box specimen.box
    in
    case current of
        Blank ->
            []

        SpatialNode node ->
            if node.box |> BoundingBox2d.intersects specimen.box then
                (node.contents |> List.filter contentHasOverlap)
                    ++ query node.nw specimen
                    ++ query node.ne specimen
                    ++ query node.se specimen
                    ++ query node.sw specimen

            else
                []
