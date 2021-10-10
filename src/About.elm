module About exposing (..)

import Element exposing (..)
import Element.Background as Background
import Markdown
import Pixels exposing (inPixels)
import ViewingContext exposing (ViewingContext)


aboutText =
    """## GPXmagic is freely provided without warranty.

## 2.4.0 update 2021-10-10

### Significant changes in managing the workspace

** https://youtu.be/iTw_Ssoe41Q **

- Prominent vertical divider can be dragged sideways using controls at top and bottom
 to change the amount of screen space given to the track views and the tools.
 The location is saved between sessions.

- The course view panes size automatically to fill space available.

- Button at top left of the first pane toggles between having one or two columns of views.

- There is a secondary position control below the views, where it was in V1.
There's still the one above the tools.

- Tool information now obscures the tools. Such is the price of having the splitter.

- Each tool pane will try to make best use of available width.

- Open tools do not jump to top of the tool stack but stay in place.
Favourites will still be at top.

### Bug fixes and small change

- _Smoothe these points in 3D_ button in _Bend Problems panel_ uses the number of segments
selected in the _Bend Smoother Classic_ panel.

- Centroid averaging does not move start and end points in track that's not a loop.

- A situation where the Drop/Clear marker controls were losing their blue colour has been fixed.

- _Splitter & Joiner_ allows you to request that each section is processed by _One Click Quick Fix_
before being written (Steve).

- Reverted to more subtle marker cones in third person view, as the big ones sometimes obscure features.

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
