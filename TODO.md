# DONE
1. Basic structures and operations. Keep it simple. OK
2. Populate and test. (Parse GPX.) OK
3. All trackpoint derivations in one pass. OK
4. Simple rendering. OK
5. Rendering with variable detail. OK
6. Graph. OK (incomplete)
7. Editing with graph
Indexing preserving. OK.
Position preserving. OK. 
8. New controls layout? No, minor refresh.
? Zoom + | - overlay on view instead of scroll bar (as per Map).
Simple: Auto remove sharp "folds" in loaded track.
9. Multiple viewports and views.
Turns out that elm-3d-geo prefers coordinates near the origin (due to loss of precision?).
Have patched in the Ghanian transform but need to retain base point for converting back!
Synchronize window focal points (Display option?)
10. Map integration. May have found how to have permanent DIV for map with show/hide!
Need a track position scroller!
Popup explanatory text for accordion. (Display in Info view?)

# TODO

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
> Only invoke and consume previews from open tabs.;
> 
> Put +/resize ViewPane controls back at top left of pane zero.

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

