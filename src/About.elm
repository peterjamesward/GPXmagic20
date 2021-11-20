module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import GeoCodeDecoders exposing (IpInfo)
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText : Maybe IpInfo -> String
aboutText ipInfo =
    """## GPXmagic is freely provided without warranty.

## 2.7.3 update 2021-11-20

- Fix for road curtain on track below sea level.

- Fix for wrong click detect logic in Plan view.

- **New tool alert** Next to _Bend smoother classic_ now lives _Curve Former_.
This will force a bend onto the radius you choose, provided it can find transitions
(with a different radius) back onto the route.

Use the Orange marker to position roughly, and the two-way drag control for fine positioning.
Optionally use the Orange and Purple markers to limit action if tracks are very close.
Optionally pull outlying point onto the radius.

It will optionally smooth elevation change over the new region of track. Otherwise, it
will attempt to follow, by interpolation, the original elevations.

Yes, it sounds complicated; there's a video to explain: https://youtu.be/DjdwAFkgw2o

- Orange and Puple markers easier to see when zoomed out in Profile (they are always
the same size on the screen somehow).

- Minor errors fixed and removal of some techical debt.

## Nice, Pete. I'd buy you a coffee, but I live in """
        ++ (Maybe.map .city ipInfo |> Maybe.withDefault "a far-away land.")
        ++ """

Don't worry, it's all possible with the Interthingy.
Use the not so subtle yellow button at the top.
Donations will be passed on to our local hospice.

## Guidance on use

Load a local GPX file by clicking on the thus-labelled button.

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


viewAboutText : ViewingContext -> Maybe IpInfo -> Element msg
viewAboutText view ipInfo =
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
            [ html <| Markdown.toHtml [] (aboutText ipInfo) ]
