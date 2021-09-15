module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## GPXmagic is freely provided without warranty.

## 2.2.9 update ****

- Upgrade Mapbox component to v2.4.1. Performance improvements, bug fixes. Terrain!

- Scale track now goes from 0.1x to 10x. You can use it more than once.
Also indicates expected route length on button.

- New feature to fetch track point elevations from Mapbox elevation data source.
This is most useful after a recentre, rotate or scale. Accuracy is unknown.

- __USAGE TIP:__ If you're trying to re-centre a route, it's best if you first click the padlock
on the top-right of the map view. This stops map clicks from re-centering the map on the nearest
track point when you click.

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
