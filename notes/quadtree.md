
# On the selection of data structures

The "natural" data structure for GPXmagic is a list of track points. That, after all, is what a road is. 
Most of the time this is perfectly fine, but it's worryingly expensive for accessing a single track point
at random. 

In particular, I need to find a track point nearest to where the user clicks on the 3D image. 
Since I leave the rotation and zooming to the WebGL component, I have no direct way of interpreting the 
click location. The best I can do is to create a "ray" from which I can measure the distance of any point.
Given a ray and a list of points, my first-pass solution was simply to find the point with the least distance,
easy to write:

```elm
trackPointNearestRay : List TrackPoint -> Axis3d Meters LocalCoords -> Maybe TrackPoint
trackPointNearestRay track ray =
    track
        |> List.Extra.minimumBy
            (Length.inMeters << distanceFromAxis ray << .xyz)
```

It necessarily scans the whole list. This is acceptable for hundreds or even a few thousand points but clearly not
elegant or performant as the lists grow. Still, for most tracks, it was fast enough to be responsive to the user's
input that there was no urgent need to change it. (Hoare's original statement was "We should forget about small 
efficiencies, say about 97% of the time: premature optimization is the root of all evil.")

Time passed, and I'd been figuring out a neat way of detecting intersecting track segments. A simple
list-based approach to this requires time proportional to the square of the number of points. What may work 
for one hundred points will take a million times longer for a hundred thousand. Time to investigate data structures.

To cut this story short, there are several variants of quadtree that might work but all seemed oddly complex
to implement. I decided to try a really, really simple approach. Imagine you are responsible for a rectangular
region and must keep track of all road segments in your region. Further suppose you divide your region into
four quadrants (say: NW, NE, SE, SW). You implement the following rule:
> If a road segement's bounding box fits entirely within a quadrant, delegate responsibility to that quadrant.
> If it spans two or more, add it to a list at my level.

With the addition of a rule that imposes a limit on sub-division such that regions smaller than (say) 10 metres
are not sub-divided, this is a well-defined and easily implemented tree structure. Sure, it may be wildly
out-of-balance for some routes, but is pretty well-behaved for most real-life routes.

Searching is simple: given a bounding box, I look for overlaps with entries at my level, and I ask my
delegates to do the same.

So much simpler than all these clever structures; significantly better than a full search, but easy enough
to explain on the back of an envelope.

I then figured I could use this to help with finding the nearest to a ray. It turns out to be fairly simple.
If I project the ray into the XY plane, I can query the data structure by asking "which of your bounding 
boxes intersect with this line?"

We can do better, though. Instead of retrieving all the interecting boxes and finding the closest, why not
"push down" the query so that the recursive traversal keeps track of whatever is "nearest" and merely returns 
the single result?

Here's the "data structure traversal" that uses a `valuation` function that we seek to minimize:

```elm
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
```

The "application side" call is pretty neat:

```elm
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
```

What's really exciting is that having this structure to hand has opened a whole plethora of application
possibilities that were not practicable without an efficent index. But, I'm glad that I waited until there
was a clearly defined need, and this was not in the category of "premature optimization".


