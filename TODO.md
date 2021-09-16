

# BUGS

Why is sea level wrong after elevation fetch (e.g. Hillingdon -> Alps)?

Typing in to file name is slow. No idea why, code is simple.
Possibly something expensive in the view hierarchy, though I think that would be more obvious.

# TODO

Use mapbox plug-in to draw a route directly on the Map.
https://github.com/mapbox/mapbox-gl-draw

1. New GPX entry mode button
2. Shows Map view only with no track
3. Send command to JS to enable drawing
4. Each new point sends list of points (with elevations) to Elm
5. Elm builds the route
6. Another button to finish?

Plan from one route, elevation from another.

Appended route elevation adjusted and contiguous.

Strava segment blend elevation rather than just Paste (optional).

? Flythrough to respect any azimuth & elevation applied when stationery.

"One useful elevation tab tool might be the ability to apply a slope difference to a range of points."
(I don't understand this suggestion.)

# Not doing

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

