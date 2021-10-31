module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## GPXmagic is freely provided without warranty.

## 2.5.0 update 2021-10-31

- **New tool alert.** Move and Stretch is nudge, supercharged. In Move mode, you can use the circular control
to horizontally displace a marked section of track by up to 100m (more than 300ft). In Stretch model, you
must use the slider to position a new White marker between the Orange and the Purple. The 2D control will
then move this marker, but all point between it and the Orange and Purple will move in proportion. You can
use this, for example, to "open up" a set of bends that don't render well in Magic Roads.

- Note: Previews in Map view have a 1/10 second delay, please be patient.

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
