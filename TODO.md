

# BUGS

_SVG parser should report errors. At least summarise outcome._

**Classic bend smoother used to show segments. What's happened?**

# TODO

@Jason Hurst:

    Can you enable a custom pallet that would allow users to drag their 
    preferred functions onto that pallet and arrange them in their preferred order 
    for that GPXMagic session?
    I appreciate that you wouldn't be able to store user preferences without 
    requiring logins and that takes you down the dark path of privacy legislation 
    and security. So a 'per session' pallet seems a reasonable compromise.
    
Should be able to do this with https://package.elm-lang.org/packages/billstclair/elm-localstorage/latest/
Not sure how to configure pallette. Perhaps "Favourite" on each tool, with ordering & removal subsequent.
May need some modality: normal tools, configure favourites, use favourites.

Maybe time to improve structure of Accordion, so that it becomes more generic and self-administrating.
That is, all Accordion (tab-level) messages are handled in the module. Obviously, tool-level messages
are dispacthed to the relevant tool by Main. (No advantage moving all this to Accordion.)

New thought. Not having a separate Favourites, just allow reordering of the accordion.
Possibly with a (...) area at the end so that some can he parked there.
Or even easier, when you Star a tool, collapsing it doesn't send it back into the morass.

**Might be able to deal with this just in terms of display order, with some visual tweaks.**

    1 Starred Open tools.
    2 Starred Closed tools.
    3 Separator
    4 Other Open tools.
    5 Separator = Option to collapse Closed tools.
    6 Other Closed tools.

Still use localstorage to save state of all tools.
This lacks re-ordering but is 80% of value with 20% of effort, so maybe worth as an interim for feedback.

--

Plan from one route, elevation from another?

Appended route elevation adjusted and contiguous.

Strava segment blend elevation rather than just Paste (optional).

? Flythrough to respect any azimuth & elevation applied when stationery.

"One useful elevation tab tool might be the ability to apply a slope difference to a range of points."
(I don't understand this suggestion.)

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

