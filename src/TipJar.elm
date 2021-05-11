module TipJar exposing (info, tipJar)

import Element exposing (..)


info =
    """## Tip Jar

GPXmagic is provided free for personal use.

There is no tracking, no adverts, no cookies.

Any donations are welcome, gratefully received, and passed on
to our local hospice which provides wonderful care and receives
little if any government funding.

PayPal deducts a small handling fee, but there you go.
"""


tipJar =
    row [ padding 10, spaceEvenly, centerX, centerY ]
        [ newTabLink []
            { url = "http://paypal.me/peterjamesward"
            , label =
                image [ width <| px 200 ]
                    { src = "images/tipjar.jpeg"
                    , description = "Tip jar QR"
                    }
            }
        ]
