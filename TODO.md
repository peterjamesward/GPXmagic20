

# BUGS


# TODO

Report GPX parse errors, eg no elevation.

Interpolate with no markers should apply to whole track.

One-click quick-fix.
    - Simplify until mean density >= 25 meters, empirically.
    - Maximum slope 15% up & down.
    - Interpolate to max 10m spacing, say.
    - Centroid x N, N = 3?
    - NEW -- auto local bend smoothing for gradient (10%) and bend (20%) problems.
    - Write with same file name (OS will append -1)
    - Button goes in the top bar, not the accordion.


STUFF FOR STEVE

    Prep:
        Send relevant Options through with Msg.
        Save the Msg in the Undo list.
    
    Track splitter:
        1. Split track into selected number of equal sections

    Track combiner:
        Appends new track to current
    
    Batch operation:
        1. Define list of (whole track) operations
        2. Apply over directory
        3. Appends if possible when less than theshold distance remains.

---

Strava segment blend elevation rather than just Paste (optional).

Bezier approximation should respect markers.

? Flythrough to respect any azimuth & elevation applied when stationery.

One useful elevation tab tool might be the ability to apply a slope difference to a range of points.

Working offline? (Mongoose server?)

# Not doing

Touch screen? No.

LIDAR? No.


