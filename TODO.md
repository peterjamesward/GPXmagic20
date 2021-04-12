# PLAN

1. Basic structures and operations. Keep it simple. OK
2. Populate and test. (Parse GPX.) OK
3. All trackpoint derivations in one pass. OK
3. Simple rendering. OK
4. Rendering with variable detail. OK
5. Graph.

ACTION TODAY
Save (up to) two bearings on each point, not the average.
Easier then to find egregious bends.
Maybe ditto for gradients, while we're at it.

6. Editing with graph
> Try basic edits:
> PLAN FOR UNDO/REDO.
> NUDGE ON GRAPH
> NUDGE PREVIEW.
> Index preserving (e.g. Nudge)
> Position preserving (e.g. Delete, Insert)

6. Multiple views. (Dynamically sized WebGL holes?) {Yes, just a list of them.}
7. Message wrappers for edit messages.
8. New controls layout. (Perhaps: single point | range | whole track)
9. Map integration.
9. OAuth integration.

# TODOs

Consider allowing for more than one open GPX track. 
For super advanced graph stitching.

New bend smoother - four point external centroid (convex only, not 'S').

New bend smoother - replaces each point with circular arc, but with two variants:
a: Arc2d (like current) - "constant slope" strategy
b: Arc3d - "preserve gradients" strategy
This will be good for autosmooth - insert enough points to bring transitions below user thresholds.
(It will be better that GPXsmoother).

Option to pin Accordion tab open; user decides how many, and which, to have open.

