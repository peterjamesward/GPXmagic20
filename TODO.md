

# TODO

No Nudge preview in Profile!

## FIRST:
> Move all the tools "under" Accordion. --No. Because there's also ViewPanes.
> Tools can return their preview (List Trackpoint) as Action (1 or 2 below).
> Universal adoption of message Actions, to be handled as low as possible in hierarchy
> but also passed up if necessary:
> 1. Track geometry change (index preserving, position preserving);
> 2. Track options change (hence rebuild required);
> 3. Markers moved and recentre; (previews from open tabs)
> 4. Markers moved, no recentre; (previews from open tabs)
> 4. Nothing to do.   

## THEN
11. Bring all the edit stuff over. (Fixing all the tab layouts.)

12. Strava OAuth integration. (Could we use a popup to avoid losing state?)

# BACKLOG, being enhancements or just finishing up ...

DSL for navigating the graph ("tulips" for directions, heading?)
Convert from Graph is snafu'd.

Working offline?

!! Beware bounding box on tracks crossing the international date line !!

New bend smoother filter - four point external centroid (convex only, not 'S').

(Another) New bend smoother - replaces each point with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy
This will be good for autosmooth - insert enough points to bring transitions below user thresholds.
(It will be better that GPXsmoother).

Bend & Gradient problems to show distance not index.
(These need to be pre-calculated on load or track change.)

Option to limit gradients (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

Consider allowing for more than one open GPX track.
(For super advanced graph stitching.)

Save & Restore display options and pane layout.

