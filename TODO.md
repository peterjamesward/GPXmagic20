

# TODO

**Splash screen** - Shall we move the About view over?

**Graph** - the thing that kicked it all off. 
a) Applying edits to canonical graph.
b) Turning it back into a route.
c) DSL for re-routing (including turn-arounds).

**New bend smoother filter** - four point external centroid (convex only, not 'S').

New tool to **limit gradients** (range or whole track), retaining elevation of marked points.
(There are options. None are perfect, but we can be easier to use than competition.)

Working offline?

!! Beware bounding box on tracks crossing the international date line !!

Consider allowing for more than one open GPX track.
(For super advanced graph stitching.)

Save & Restore display options and pane layout.

# OLD STUFF TO CHECK

1. Elevate all points by set amount to correct Strava or route planner inaccuracy

2. Set max slope either over defined range of points or over whole route

3. Set minimum slope as -25% as that is what RGT will do anyway

4. Next one is part of the smoothing I do in GPX smoother. 
   May not be for everyone, but I normally set a slope difference between all trackpoints at somewhere 
   between 0.3 and 1% to guarantee a smooth ride. 
   It almost inevitably alters the profile though and I then use the original 
   elevation profile to make adjustments, so this would only be useful to me if you 
   could have a reference point to then try to adjust the slope back to the right shape, but smoother. 
   So either something from veloviewer or the original profile in the background

5. One useful elevation tab tool might be the ability to apply a slope difference to a range of points.
