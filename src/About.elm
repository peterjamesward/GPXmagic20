module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## Thank you for trying GPXmagic.
    GPXmagic is freely provided without warranty.

## 2.2.0 update 2021-08-31

- One-click Quick-fix for the time-crunched. Aims to make "most" routes "rideable" with no effort by:
    1. Reducing average track point density (this is good for removing noise from recorded IRL rides);
    2. Limiting gradients to no more than 15% up or down;
    3. Interpolating so maxmimum track point spacing is 10 metres;
    4. Five rounds of 50% centroid averaging smoothing.
- Elevation defaults to zero, so you can read files with no elevation data.
- Interpolate applies to whole track if no range is selected.

## Donations

**YES, PLEASE!!**

Donations will be passed on to our local hospice. Just use the TipJar, it's available 24x7.

## Guidance on use

Load a local GPX file by clicking on the aptly-labelled button.

Or connect to Strava by clicking on the equally-apt brand-compliant button and
authorize GPXmagic to access your routes. You can then paste in a URL for a
Strava route and get the GPX by clicking "Fetch route".
The Strava connection is valid for six hours.

**Remember to save your changes often.**
The Save button writes to your download folder only (this is a security limitation of browsers).

## Source code

v2 source code is open-source. See https://github.com/peterjamesward/GPXmagic20

## Legally required notices

Compatible with Strava, for the purpose of loading route and segment data.

Your IP address is logged for the purpose of aggregate usage recording; no personal details are stored.

No cookies are used.

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
