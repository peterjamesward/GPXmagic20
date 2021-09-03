module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## Thank you for trying GPXmagic.
    GPXmagic is freely provided without warranty.

## 2.2.2 update 2021-09-03

- TipJar replaced by "Buy Me A Coffee" button, because.
- Tidy up of orange and purple marker positions after changes.
- Not allowed to delete the whole track.
- Tweaked One-click Quick-fix:
    1. Reduce average track point spacing to remove IRL noise
    2. Apply Bezier approximation
    3. Apply centroid smoothing three times

## Donations

**YES, PLEASE!!**

Donations will be passed on to our local hospice.
We have changed to "Buy Me a Coffee".
Use the not so subtle yellow button at the top.

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
