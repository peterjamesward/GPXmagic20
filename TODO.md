
# BUGS

SVG parser should report errors. 
At least summarise outcome.

Sometimes some buttons losing style in Safari 15.0.
This has been remarked by others on Slack elm-ui channel.
May not be fixable. Maybe isolate test case?

---

# TO-DO

Factor the PortMessage handling out of main::update.

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

