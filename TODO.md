

# TODO

**Graph**

BUG - Initial view still in 8:6 ratio.

BUGS - some edits are breaking the route.
Testing with Hillingdon (a trivial graph)
    + Bend smoother barfs
    + Gradient smoother loses the first or last point of the route?
    + Ditto centroid filter
    + Bezier splines not confined to selected range
    + Probably should disable Loop tools completely
    + Nudge (even!) messes with points outside selected range


-- 2.1.0 ships here --

**Terrain**. Perhaps with quadtree.

With Quadtree, **LIDAR** may be possible.

**Nudge** with a variable size brush, with damping. 
Visible by the preview, where we see a "fade" back to the normal track.

**New bend smoother filter** - four point external centroid (convex only, not 'S').

New tool to **limit gradients** (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

Flythrough respects any azimuth & elevation applied when stationery.

One useful elevation tab tool might be the ability to apply a slope difference to a range of points.

Working offline?

Beware bounding box on tracks crossing the international date line !!
