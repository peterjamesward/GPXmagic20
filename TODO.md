
# BUGS

Charts not being refreshed after Edit/Undo/Redo.

---

# WIP

---

# Backlog

## Samir
Is it possible to make a button/panel to find all curves with radius less then "insert number", 
as those are the ones that I would adjust for smooth riding in RGT? Thanks in advance.  
> Involves direction changes v. distance changes across whole track (window calc).  
> Have radius and angle change sliders. This will allow people to look only for hairpins, say.  
> Add the radius slider to Bend Problems tab, backwards compatible at small radius.
> NB, can easily fast-forward "nearly straight" sections.

## Use elevation from second route, starting at marker.
> This should be a neat two-way merge sort of track points based on distance from start.  
> We have interpolation code in Flythrough (and Curve Former), so it's a doddle.  
> Applies elevation relative to start elevation, of course.  
> Option to have "cross-fade" at the end (possibly hundred of metres).  

## Improve start/end coincidence testing in Intersections.
> Uses the TP.index here to see if they are adjacent on the route rather than just by chance.  
> Better, still too many now. Need closer look at Dan's test file.

## TECH DEBT

At least three similar forms of new Track creation.

Don't copy information into ViewContext that doesn't belong there. 
> Only Flythrough?  
> Safer & easier to pass through the data needed.  
> Now I know how to write type signatures that don't require the whole Main.Model!

Switch currentPoint to : Int, avoid stale state worries.

---

# v3 candidates

## Racket native app!

## (OR) Haskell native app!
> https://www.hgamer3d.org/Entities.html
> https://speakerdeck.com/mchakravarty/playing-with-graphics-and-animations-in-haskell

## Rationalise tools (again)
E.g. separate info tabs from edit tabs. Find some themes, or contexts.

## Info popups on 3D views (V3)
Seems easy now we have spatial index, SVG charts, OnHover.
> https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/

## **Stratified** data structures instead of heavy-weight TrackPoint.
1. Track points as read from GPX;
2. Track points converted to XYZ;
3. Road segments derived from adjacent track points;
4. Inflection points (shall we call them?) derived from adjacent road segments;
5. Profile;
6. "Problems";
7. Spatial index (of roads, of inflection points);
8. Elided points for rendering;
9. Terrain;
10. Graph (erks me that this is so different, but it is).

> Aim/hope is to reduce updating and hence reduce memory churn.
> How to manage minimal updating? Version numbers? Will it just fall out?
> We edit in the '2' level, and only fall back to 1 for output (apart from Map actions)
> Anything in '3', '4', '5' is for info, and derived after each edit.
> N.B. minor optimisation is to only re-derive from start edit forwards.

Allow Tools to place a control on the view pane. When selected, drag messages are
forwarded to the Tool. Would, for example, allow direct dragging on the view.

In similar vein, we could put the Tool submodels into a dict keyed by Msg subtype
then Main.update becomes nice dispatch system. (Unlikely.)

## **Terrain** on empty squares; make it related to context of neighbours.

---

# Not doing

## Satellite map option (only if possible in v3, which may use Open Streetmap, so no.)

**Lane separation** on out and back sections (?)
> Without need for Graph. This could just be simple +/- offset withing marked region.
> Just use Nudge or Move & Stretch.

Change Track.spatialIndex to be road-based, so we can re-use for Terrain?
> It may remove any "misses" in click detection btw.
> **Note** that intersection testing relies on incremental build of its own index!
> **Also** last point becomes special case and much ugliness ensues.

Visual Styles options layout not wrapping.
Turns out to be Safari!

Investigate **derivatives** of Vector3d and see if anything useful emerges.
> This will be editing "blind". Could be fun.
> Limiting 1st or 2nd derivatives.

**Investigation** Use of Canvas.
> Happy now with spatial index.

- https://github.com/joakin/elm-canvas
- https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/

**Profile** click detect should not do a linear search.
> It's just not necessary. It's stupidly fast anyway.

**Timestamps**: Is it possible to preserve GPX time stamps? **DO THIS**
Obvs certain edits will largely invalidate, but we could preserve any we read in.
<trkpt lat="51.6159740" lon="-0.3014110">
<ele>97.4</ele>
<time>2021-10-28T07:53:33Z</time>

Try **electron** for native wrapper. See if multiple windows possible.

Strava segment blend elevation rather than just Paste (optional).

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

