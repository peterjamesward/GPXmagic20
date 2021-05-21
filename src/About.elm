module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## Thank you for trying GPXmagic.
    GPXmagic is freely provided without warranty.

## 2.1.23 update 2021-05-20

- Dragging more consistent between 1st and 3rd views.
- Rotate in Plan.

## About Version 2

Version 2 is a 90% re-write.
The code base, whilst imperfect, is a much better base for further work.
I've tried to keep the overall look consistent, but with some fairly obvious changes.

### Big things

* Convert to canonical form and make up your own route (Graph Theory)
* You can have up to four view panes open
* Views can be re-sized (but they're all the same size)
* You can have more than one tool open. Click to close. Open tools float to the top.

### Medium things

* There's a new single "3D" point smoother that replaces the v1 "chamfer" and works for bends and gradients
* Autofix on Bend and Gradient problem tools uses the new 3D bend smoother
* Double clicking a point will centre all views, but you can detach a view by clicking the padlock
* Zoom scroll bars replaced with + & - on views, consistent with Map. Mouse wheel also zooms.
* Map is "drag to move point" mode only
* Reduced carbon footprint by not doing the same thing twenty times a second
* Terrain is so fast you can edit with it turned on (but it's simple)

### Small things

* The map is pre-loaded, so switching is quicker (but it's only allowed in the first view pane)
* New "Steep climbs" tool helps you locate, um, steep climbs
* Rotate and re-centre your route (if you're called Steve)
* Positions of both pointers are shown in meters
* Problems are labelled by distance in meters, not point number
* "Zero sliders" and "fade range" on Nudge tool
* Bend and Nudge previews are shown only when the tools are open
* Similarly, closing the Flythrough tool stops the flythrough
* Short help text for each tool, accessed by the (i) button

## Donations

A few users have asked if I would accept some payment.
**YES, PLEASE!!** Donations will be passed on to our local hospice.

## Guidance on use

Load a local GPX file by clicking on the aptly-labelled button.

Or connect to Strava by clicking on the equally-apt brand-compliant button and
authorize GPXmagic to access your routes. You can then paste in a URL for a
Strava route and get the GPX by clicking "Fetch route".
The Strava connection is valid for six hours.

**Remember to save your changes often.**
The Save button writes to your download folder only (this is a security limitation of browsers).

## Source code

v2 source code is **not** currently open-source. If you want access, please ask.

## Legally required notices

Compatible with Strava, for the purpose of loading route and segment data.

Your IP address is logged for the purpose of aggregate usage recording; no personal details are stored.

No cookies are used, though they may not be true for the site as a whole.

> _Peter Ward, 2021_
"""


viewAboutText : ViewingContext -> Element msg
viewAboutText view =
    let
        ( w, h ) =
            view.size
    in
    row
        [ centerX
        , Background.color <| rgb255 220 220 200
        , clipY
        , scrollbarY
        , padding 20
        , width <| px (inPixels w)
        , height <| px (inPixels h)
        ]
        [ paragraph
            [ width <| px (inPixels w)
            , height <| px (inPixels h)
            ]
          <|
            [ html <| Markdown.toHtml [] aboutText ]
        ]
