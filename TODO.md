
# BUGS

Initial zoom in Profile is broked.

---

# TO-DO

**DEBT**: Refactor PostUpdateAction in Main. (Move & Stretch is cleaner perhaps.)

**DEBT**: Don't update state in ViewContext, safer to pass through the data needed.

**DEBT**: Factor the PortMessage handling out of main::update.

**DEBT**: Revert currentPoint to : Int, avoid stale state worries.

**NEW**: Use elevation from second route, starting at marker.
- This should be a neat two-way merge sort of track points based on distance from start.
- We have interpolation code in Flythrough, so it's a doddle.
- Applies elevation relative to start elevation, of course.
- Option to have "cross-fade" at the end (possibly hundred of metres).

---

# Not doing

**NOTE**: I may have just realised that intersection testing is not completely dissimilar to
terrain generation with recursion, bounding boxes and cheap BB overlap tests.
Only need segment intersection at the last stage. Not sure what to do with this thought,
it's a lot of work for slight benefit. Might be good coding exercise though.

**THOUGHT**: New tab for Overlaps (initially 2D, maybe 3D aware later).
- Highlights overlapping sections (perhaps just white circles or bbox)
- Possible adjustments by moving sections up/down/left/right (arrow keys?) (at some quadtree level?)
- Possible adjustments by pivoting around marker.
- For road intersections, move up/down/match, or create common TP for graph node.
- With expanded bounding boxes, can test for close roads also.
+ **NOTE** most of this is obviated by new Move and Stretch, apart from common TP.

**Timestamps**: Is it possible to preserve GPX time stamps? **DO THIS**
Obvs certain edits will largely invalidate, but we could preserve any we read in.
<trkpt lat="51.6159740" lon="-0.3014110">
<ele>97.4</ele>
<time>2021-10-28T07:53:33Z</time>

Try **electron** for native wrapper. See if multiple windows possible.

Strava segment blend elevation rather than just Paste (optional).

Better terrain by slightly smarter rendering of each quadrant based on its context.

Working offline? (Mongoose server?)

Touch screen.

LIDAR.

Flythrough to respect any azimuth & elevation applied when stationery.

--- Also not doing, as unlikely to be any better than Bezier apprx.

Consider colouring TP by curvature.
This may lead to way of identifying (e.g.) sweeping bends, straights, hairpins.
That might then lead to more intelligent automated smoothing, specialised by region.

Start by computing "circle of curvature" for each non-end track point. (Trivial with elm-geometry.)
Via Visual Options or a new view, visualise these circles (or just current node, so we can see movement).

Is it right or reasonable to assert that the centre of these circles should move continuously,
and further that we should limit the rate at which it moves (or accelerates)?
This is more general than thinking about track regions.
(It's closer to Dan C's idea of using equiangular spirals as the basic construct.)

May require some GA/GP search to find an approximation that is suitably close (by what measure) to the route.
But this revives the idea of the user being able to control preferences.

BTW, should consider uphills different to downhills; it's track as function of time, not distance.

---

