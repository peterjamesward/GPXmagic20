
# BUGS

---

# TO-DO

SVG: Implement:
- transform="scale(0.26458333)
- transform="matrix(0.26458333,0,0,0.26458333,90.802455,0.5260581)
- 
- drawing-1 is not good, despite no transforms. 
- Think the initial 'm' is not what I think, second path is "m [ x,y ]"

DEBT: Factor the PortMessage handling out of main::update.

DEBT: Revert currentPoint to : Int, avoid stale state worries.

Use elevation from second route, starting at marker.

Detect crossings and insert under/overpass.

Try to isolate Safari problem.

Try electron for native wrapper. See if multiple windows possible.

---

# Not doing

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

