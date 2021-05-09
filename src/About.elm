module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## Thank you for trying GPXmagic.
    GPXmagic is freely provided without warranty.

## 2.1.1 update 2021-05-08

- Ability for user to modify route using Graph tools.
- Enhanced contrast on tab labels.
- View aspect ratio now 8:5 (was 8:6).
- Loading new route does not affect pane layout
- Grevious error in projection corrected
- Tools will work in Graph mode and update the canonical edges

Version 2 is a 90% re-write.
The code base, whilst imperfect, is a much better base for further work.
I've tried to keep the overall look consistent, but with some fairly obvious and major changes.

* Convert to canonical form and make up your own route (Graph Theory)
* You can have up to four view panes open
* Views can be re-sized (but they're all the same size)
* Double clicking a point will centre all views, but you can detach a view by clicking the padlock
* Zoom scroll bars replaced with + & - on views, consistent with Map. Mouse wheel also zooms.
* The map is pre-loaded, so switching is quicker (but it's only allowed in the first view pane)
* Map is "drag to move point" mode only
* You can have more than one tool open. Click to close. Open tools float to the top.
* Problems are labelled by distance in meters, not point number
* Positions of both pointers are shown in meters
* There's a new single "3D" point smoother that replces the v1 "chamfer" and works for bends and gradients
* Autofix on Bend and Gradient problem tools uses the new 3D bend smoother
* "Zero sliders" button on Nudge tool
* Bend and Nudge previews are shown only when the tools are open
* Similarly, closing the Flythrough tool stops the flythrough
* New "Steep climbs" tool helps you locate, um, steep climbs
* Possible energy saving by not doing the same thing twenty times a second
* Short help text for each tool, accessed by the (i) button

v2.1 is slated for a few weeks with some new and exciting tools and (inevitably) bug-fixes.

## Donations

A few users have asked if I would accept some payment.
Thank you kindly, but I do this for fun and challenge; like cycling.
In case you feel the urge, I have added a Tip Jar QR that allows you
to contribute towards a fund that will go to our local hospice.
Or you can just give a gift to any cause; it's the spirit that counts.

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
