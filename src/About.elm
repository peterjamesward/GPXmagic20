module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## GPXmagic is freely provided without warranty.

## 2.4.12 update 2021-10-20

- FIXED: Fixed error in SVG absolute spline rendering. (Yeah, I know.)

- UPDATE: SVG import is better, at least for some files. It will extract "path" information and
load directly into the track editor. You'll want to use _Lift & Shift_ to move, rotate and scale.
Only supports M,m,L,l,C and c SVG commands, until someone needs more.

## Can I support this work?

**YES, PLEASE!!**

Donations will be passed on to our local hospice.
We have changed to "Buy Me a Coffee" as a payment provider.
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

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

No cookies are used, though many chocolate digestives were consumed whilst writing.

> _Peter Ward, 2021_
"""


viewAboutText : ViewingContext -> Element msg
viewAboutText view =
    let
        ( w, h ) =
            view.size
    in
    el [] <|
        paragraph
            [ width <| px (inPixels w)
            , height <| px (inPixels h)
            , padding 20
            , Background.color <| rgb255 220 220 200
            , clipY
            , scrollbarY
            ]
        <|
            [ html <| Markdown.toHtml [] aboutText ]
