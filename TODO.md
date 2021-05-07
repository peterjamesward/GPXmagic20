

# TODO

**Graph**
_DONE_ a) Applying edits to canonical graph.
_DONE_ b) Turning it back into a route.
_DONE_ c) DSL for re-routing

d) Convert back to track using offset and adding U-turns as needed.
e) Radius bends at nodes using 3D smoother.
f) Always highlight route section just added (need logic for highlighting in list).

-- 2.1.0 ships here --

Terrain. Perhaps with quadtree.

**New bend smoother filter** - four point external centroid (convex only, not 'S').

New tool to **limit gradients** (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

Flythrough respects any azimuth & elevation applied when stationery.

One useful elevation tab tool might be the ability to apply a slope difference to a range of points.

Nudge with a variable size brush, with damping. Don't know how we'd show this.
Possibly only visible by the preview, where we see a "fade" back to the normal track.

Working offline?

Beware bounding box on tracks crossing the international date line !!
