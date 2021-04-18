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
8. New controls layout? No - folk will complain.
? Zoom + | - overlay on view instead of scroll bar (as per Map).
Simple: Auto remove sharp "folds" in loaded track.
9. Multiple viewports and views.
Turns out that elm-3d-geo prefers coordinates near the origin.
Have patched in the Ghanian transform but need to retain base point for converting back!

# TODO

> Need a track position scroller!
> Popup explanatory text for accordion.
> Synchronize window focal points (Display option?)

10. Map integration. (check with variable size panes!)
11. Bring all the edit stuff over.
12. OAuth integration. (could we not use a popup to avoid losing state?)

# BACKLOG, being enhancements ...

DSL for navigating the graph ("tulips" for directions?)

Working offline.

!! Beware bounding box on tracks crossing the international date line !!

New bend smoother filter - four point external centroid (convex only, not 'S').

(Another) New bend smoother - replaces each point with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy
This will be good for autosmooth - insert enough points to bring transitions below user thresholds.
(It will be better that GPXsmoother).

Bend & Gradient problems to show distance not index.

Option to limit gradients (range or whole track), retaining elevation of marked points.

Consider allowing for more than one open GPX track.
(For super advanced graph stitching.)


