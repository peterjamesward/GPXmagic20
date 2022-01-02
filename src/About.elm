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

## 2.9.0 update 2022-01-02

**NOTE:** Version 2.7.13 [available here](https://s3.eu-west-1.amazonaws.com/stepwiserefinement.co.uk/GPXmagic_2_7_13/index.html).

With acknowledgements to John Bytheway:

- When _Bend problems_ or _Gradient problems_ tabs are open, the corresponding track points are
highlighted in yellow on the First, Third and Plan views.

- Under _Looped Track maker_ there are buttons to make out and back routes.
These also add a loop back to thet start; delete if not required. I recommend doing some
basic smoothing _before_ you do this, as doing it later raises the risk of getting height
variations.

- The text and the tabs have some colour variation to reduce the "sea of green" effect.
Where possible, these should correlate with previews on the view. There's no particular
theme, and it's subject to change.

When you encounter problems, **please** try to send me instructions to recreate them.
I spend ages putting all these bugs in, and I need your help to isolate and fix them!

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
