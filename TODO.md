

# BUGS

SVG parser should report errors. At least summarise outcome.

Sometimes some buttons losing style.
Also notice major rendering differences with optimised JS, which is bizarre.
Optimised code is shit on Chrome as well. No, not so much.

Too fing confusing. Can I do my own splitter? Can I just use a scroll with custom thumb? Anything?

# TODO

Save pane layout and views.

Can the split element itself be draggable?

@Steve -- When splitting roads for output, I think it would be useful to the end user if each road 
carried a sequence prefix: 1-Road1, 2-Road2, 3-Road3 etc. 

Store various minor control settings, now we know how.

Clean up splitter code with less swapping between HTML and elm-ui?

Plan from one route, elevation from another?

Appended route elevation adjusted and contiguous.

Strava segment blend elevation rather than just Paste (optional).

? Flythrough to respect any azimuth & elevation applied when stationery.

# Not doing

Better terrain by slightly smarter rendering of each quadrant based on its context.

Working offline? (Mongoose server?)

Touch screen? No.

LIDAR? No.

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

