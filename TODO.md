

# TODO

**Graph**

BUG - Nudge on round and round is moving a nearby node. 
Probably the 'applyIndexPreservingEdit'!

BUG - Recalculate edge node pair list in graph after node preserving edit
(which looks like just bad list splicing)

BUG - Track load should not change view pane layout at all.

TODO - Probably should disable Loop tools completely in Graph mode. 

> 2.1.0 ships here

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
