# PLAN

!! Beware bounding box on tracks near the international date line !!

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
   Option to pin Accordion tab open; user decides how many, and which, to have open.
   -- Not as easy as I thought, as accordion designed to have ONE tab open.
? Zoom + | - overlay on view instead of scroll bar (as per Map).
Simple: Auto remove sharp "folds" in loaded track.
   
> 9. Multiple viewports and views.
> Profile
> Need a track position scroller!

10. Bring all the edit stuff over.
11. Map integration. (leaves the nasty JS stuff to the end!)
12. OAuth integration. (could we not use a popup to avoid losing state?)

# TODOs, being enhancements ...

Popup explanatory text.

DSL for navigating the graph ("tulips" for directions?)

Working offline.

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


