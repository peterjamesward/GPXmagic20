

# BUGS

SVG parser should report errors. At least summarise outcome.

**Marker and Undo button styling not rendering properly sometimes.**
Perhaps only at certain zoom levels of browser, or pane size?
This happens now also with V1, so it's not recent changes -- it could be Safari 15.0!
**Try another browser!**

Padding on About pane is wrong when second pane open.

# TODO

1. **Store splitter location in localStorage.** (turns out to be awkward, **need to fork**)

2. **Review all tool panels for optimising use of available width.**

3. Dragging splitter resizes view panes. If possible.
Maybe have option to have multiple views one-up or two-up.
**Yes** use the x value from the splitter message to resize the panes including the Map.

Store various control settings, now we know how.

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

