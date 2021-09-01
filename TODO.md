

# BUGS


# TODO

CHANGES TO 1CQF

Alternative 1QCF - two rounds of smoothing followed by Bezier approximation?? No need for gradient limiter.

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

One useful elevation tab tool might be the ability to apply a slope difference to a range of points.

# Not doing

Working offline? (Mongoose server?)

Touch screen? No.

LIDAR? No.


