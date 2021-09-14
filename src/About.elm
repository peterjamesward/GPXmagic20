module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## Thank you for trying GPXmagic.
    GPXmagic is freely provided without warranty.

## 2.2.8 update 2021-09-14

- Bend smoother classic lets you change the number of line segments used to smooth a single track point.
Setting this to 1 equates (more or less) to the old v1 "chamfer" operation.

- Under Lift & Shift you can now expand your ride by up to 100x

- New feature accessible at foot of screen: If you have an SVG graphic file, this will quickly extract the
paths from within it into separate GPX tracks. You can then load these in, position and scale them with
"Lift & Shift". Elevation is zero, mind. You can put them through popular route planners or GIS tools to
add elevation data. You may find autotracer useful for making SVG out of image files.

## Support GPXmagic development

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
            , paddingXY 20 0
            ]
          <|
            [ html <| Markdown.toHtml [] aboutText ]
        ]
