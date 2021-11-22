module SpatialIndex exposing
    ( SpatialContent
    , SpatialNode
    , add
    , empty
    , query
    , queryAllContaining
    , queryNearestToAxisUsing
    , queryWithFilter
    , toList
    , transformUsing
    )

{-
   This is a simple quadtree based method for tracking bounding boxes.
   Its only requirement is to detect overlaps, with reasonable efficiency.
-}

import Axis2d
import BoundingBox2d
import LineSegment2d
import List.Extra
import Point2d
import Polygon2d
import Quantity exposing (Quantity(..))
import Quantity.Interval as Interval
import Rectangle2d


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
    -> BoundingBox2d.BoundingBox2d units coords
    -> List (SpatialContent contentType units coords)
query current queryArea =
    -- I think it may be much faster with deep trees to avoid all the
    -- internal concatenation at every level and do it once here.
    queryInternal current queryArea (always True) []
        |> List.concat


queryWithFilter :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> (contentType -> Bool)
    -> List (SpatialContent contentType units coords)
queryWithFilter current queryArea queryFilter =
    queryInternal current queryArea queryFilter []
        |> List.concat


queryInternal :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> (contentType -> Bool)
    -> List (List (SpatialContent contentType units coords))
    -> List (List (SpatialContent contentType units coords))
queryInternal current queryArea queryFilter accumulator =
    -- We return content whose bounding box intersects
    -- with the bounding box of the specimen. We do this by looking in a relevant child
    -- or in our own list, depending on the extent of the speciment compared to our children.
    case current of
        Blank ->
            accumulator

        SpatialNode node ->
            -- Longhand writing a depth-first traversal using the accumulator.
            if queryArea |> BoundingBox2d.intersects node.box then
                let
                    fromThisNode : List (SpatialContent contentType units coords)
                    fromThisNode =
                        node.contents
                            |> List.filter
                                (\possible ->
                                    (possible.box |> BoundingBox2d.intersects queryArea)
                                        && (possible.content |> queryFilter)
                                )

                    fromNW =
                        queryInternal node.nw queryArea queryFilter (fromThisNode :: accumulator)

                    fromNE =
                        queryInternal node.ne queryArea queryFilter fromNW

                    fromSE =
                        queryInternal node.se queryArea queryFilter fromNE

                    fromSW =
                        queryInternal node.sw queryArea queryFilter fromSE
                in
                fromSW

            else
                accumulator


queryAllContaining :
    SpatialNode contentType units coords
    -> Point2d.Point2d units coords
    -> List (SpatialContent contentType units coords)
queryAllContaining current point =
    case current of
        Blank ->
            []

        SpatialNode node ->
            if node.box |> BoundingBox2d.contains point then
                [ List.filter
                    (.box >> BoundingBox2d.contains point)
                    node.contents
                , queryAllContaining node.nw point
                , queryAllContaining node.ne point
                , queryAllContaining node.se point
                , queryAllContaining node.sw point
                ]
                    |> List.concat

            else
                []


queryNearestToAxisUsing :
    SpatialNode contentType units coords
    -> Axis2d.Axis2d units coords
    -> (contentType -> Float)
    -> Maybe (SpatialContent contentType units coords)
queryNearestToAxisUsing current axis valuation =
    case current of
        Blank ->
            Nothing

        SpatialNode node ->
            let
                boxSides =
                    node.box
                        |> Rectangle2d.fromBoundingBox
                        |> Rectangle2d.toPolygon
                        |> Polygon2d.edges

                intersected =
                    List.any
                        (\edge -> LineSegment2d.intersectionWithAxis axis edge /= Nothing)
                        boxSides
            in
            if intersected then
                [ List.Extra.minimumBy (.content >> valuation) node.contents ]
                    ++ [ queryNearestToAxisUsing node.nw axis valuation
                       , queryNearestToAxisUsing node.ne axis valuation
                       , queryNearestToAxisUsing node.se axis valuation
                       , queryNearestToAxisUsing node.sw axis valuation
                       ]
                    |> List.filterMap identity
                    |> List.Extra.minimumBy (.content >> valuation)

            else
                Nothing


transformUsing :
    SpatialNode contentTypeA units coords
    ->
        (List (SpatialContent contentTypeA units coords)
         -> List (SpatialContent contentTypeB units coords)
         -> List (SpatialContent contentTypeB units coords)
        )
    -> units
    -> coords
    -> SpatialNode contentTypeB units coords
transformUsing current transform units coords =
    case current of
        Blank ->
            Blank

        SpatialNode node ->
            let
                newNW =
                    transformUsing node.nw transform units coords

                newNE =
                    transformUsing node.ne transform units coords

                newSE =
                    transformUsing node.se transform units coords

                newSW =
                    transformUsing node.sw transform units coords

                childContents =
                    [ newNW, newNE, newSE, newSW ]
                        |> List.filterMap
                            (\child ->
                                case child of
                                    SpatialNode ch ->
                                        Just ch.contents

                                    Blank ->
                                        Nothing
                            )
                        |> List.concat

                newContent =
                    transform node.contents childContents
            in
            SpatialNode
                { box = node.box
                , minSize = node.minSize
                , contents = newContent
                , nw = newNW
                , ne = newNE
                , se = newSE
                , sw = newSW
                }


toList : SpatialNode contentType units coords -> List (SpatialContent contentType units coords)
toList current =
    toListInternal current [] |> List.concat


toListInternal :
    SpatialNode contentType units coords
    -> List (List (SpatialContent contentType units coords))
    -> List (List (SpatialContent contentType units coords))
toListInternal current accum =
    case current of
        SpatialNode node ->
            node.contents
                :: (toListInternal node.nw <|
                        toListInternal node.ne <|
                            toListInternal node.se <|
                                toListInternal node.sw <|
                                    accum
                   )

        Blank ->
            accum
