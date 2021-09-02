

# BUGS

Inferring marker placement is fraught amd dumb.
Each edit operation should return a Track with new pointers set, not a List TrackPoint.
HANG ON; they return a Track already.
So why am I not using the pointer positions there?????

So, again, list of operations to work through:
OK Nudge -- pointer indices unchanged
OK Delete (single node) -- back one unless at end, disallow delete last remaining node
OK Delete (range) -- near back one, far forward one, unless at extremes, disallow delete whole track
-- Interpolate -- adjust far pointer by length change
-- Straighten -- unchanged
-- Bend smoother classic -- adjust far pointer by length change
-- Bend smoother 3D (single point) -- move to new bend midpoint
-- Bend smoother 3D (range) -- adjust far pointer
-- Centroid -- unchanged
-- Bezier 1 -- adjust far pointer
-- Bezier 2 -- adjust far pointer

Note effect of Graph though; still want edits to be unaware so see what we need to retain of current code.

# TODO

CHANGES TO 1CQF

Alternative 1QCF - two rounds of smoothing followed by Bezier approximation?? 
No need for gradient limiter. Arguably more organic feel.

    -- Use current values for gradient limit, interpolation and smoothing.
    -- Apply local 3D fixes to any remaining problems.

STUFF FOR STEVE

    Track splitter:
        1. Split track into selected number of equal sections

    Track combiner:
        Appends new track (from local file only) to current
    
    Batch operation:
        1. Define list of (whole track) operations
        2. Apply over directory
        3. Appends if possible when less than theshold distance remains.

---

Consider colouring TP by curvature.
This may lead to way of identifying (e.g.) sweeping bends, straights, hairpins.
That might then lead to more intelligent automated smoothing, specialised by region.

Strava segment blend elevation rather than just Paste (optional).

? Flythrough to respect any azimuth & elevation applied when stationery.

"One useful elevation tab tool might be the ability to apply a slope difference to a range of points."
(I don't understand this suggestion.)

# Not doing

Working offline? (Mongoose server?)

Touch screen? No.

LIDAR? No.


