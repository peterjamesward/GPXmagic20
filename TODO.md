

# TODO

12. Strava OAuth integration. (Could we use a popup to avoid losing state?)

--TODO: Strava segment paste not pasting.
--TODO: Hit detect in 1st.
--TODO: Flythrough in all linked views.
--TODO: Make "click to drag" normal mode for Map.
--TODO: Bring back pan actions for views.
--TODO: Graph - the thing that kicked it all off.

# BACKLOG, being enhancements or just finishing up ...

Tab to highlight gradients over threshold (10, 15, 20, 25). (John Bytheway.)

New bend smoother filter - four point external centroid (convex only, not 'S').

(Another) New bend smoother - replaces each point with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy
This will be good for autosmooth - insert enough points to bring transitions below user thresholds.
(It will be better that GPXsmoother).

Option to limit gradients (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

DSL for navigating the graph ("tulips" for directions, heading?)
Convert from Graph is snafu'd.

Working offline?

!! Beware bounding box on tracks crossing the international date line !!


Consider allowing for more than one open GPX track.
(For super advanced graph stitching.)

Save & Restore display options and pane layout.

