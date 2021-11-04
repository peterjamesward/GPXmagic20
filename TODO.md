
# BUGS

Initial zoom in Profile is broked. Curious.
> Possibly fixed.

With terrain, are we rendering it redundantly when focus changes?
> No. Checked.

---

# TO-DO

Try **improving** start/end coincidence testing in Intersections.
> Need the TP Index here to see if they are adjacent on the route rather than just by chance.
> Simply, if you get an intersection with the one just added (previous id), ignore it.
> Better, still too many now.

**DEBT**: At least three similar forms of new Track creation.

**DEBT**: Don't update state in ViewContext, safer & easier to pass through the data needed.
> Now I know how to write type signatures that don't require the whole Main.Model!

**DEBT**: Factor the PortMessage handling out of main::update. PortController.update?

**DEBT**: Switch currentPoint to : Int, avoid stale state worries.

**DEBT**: I use too many nested 'case' forms. Must learn how to avoid this. (andThen ?)

**DEBT**: Factor out the common pattern of min, max for the pointers.

**NEW**: Use elevation from second route, starting at marker.
- This should be a neat two-way merge sort of track points based on distance from start.
- We have interpolation code in Flythrough, so it's a doddle.
- Applies elevation relative to start elevation, of course.
- Option to have "cross-fade" at the end (possibly hundred of metres).

---

# Not doing

Why not have **Terrain** draw the road segments? (If we are in Terrain drawing mode.)
> Because, why?

**Profile** click detect should not do a linear search.
> It's just not necessary. It's stupidly fast anyway.

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

