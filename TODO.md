# PLAN

1. Basic structures and operations. Keep it simple. OK
2. Populate and test. (Parse GPX.) OK
3. All trackpoint derivations in one pass. OK
3. Simple rendering. OK
4. Rendering with variable detail. OK
5. Graph. OK (incomplete)

> 6. Editing with graph
> Indexing preserving. OK.
> Position preserving (e.g. Delete, Insert)

6. Multiple views. (Dynamically sized WebGL holes?) 
   {Yes, just a list of them. Need to work out how to handle Profile, Map, Plan.}

8. New controls layout. (Perhaps: single point | range | whole track)
   Option to pin Accordion tab open; user decides how many, and which, to have open.
   Popup explanatory text.
7. Bring all the edit stuff over.
9. Map integration. (leaves the nasty JS stuff to the end!)
9. OAuth integration. (could we not use a popup to avoid losing state?)

# TODOs, being enhancements ...

DSL for navigating the graph ("tulips" for directions?)

New bend smoother - four point external centroid (convex only, not 'S').

(Another) New bend smoother - replaces each point with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy
This will be good for autosmooth - insert enough points to bring transitions below user thresholds.
(It will be better that GPXsmoother).

Bend & Gradient problems to show distance not index.

Consider allowing for more than one open GPX track.
For super advanced graph stitching.


