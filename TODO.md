

# TODO

BUG  Bend problem at 0 on Mount Coot-tha.

Use Arc3d point smoother to replace chamfer.

New bend smoother filter - four point external centroid (convex only, not 'S').

(Another) New bend smoother - replaces __each point__ with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy -- the new "chamfer"

--TODO: Graph - the thing that kicked it all off.

DSL for navigating the graph ("tulips" for directions, heading?)
Convert from Graph is snafu'd.

Option to limit gradients (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

Working offline?

!! Beware bounding box on tracks crossing the international date line !!

Consider allowing for more than one open GPX track.
(For super advanced graph stitching.)

Save & Restore display options and pane layout.

