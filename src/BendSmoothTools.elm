module BendSmoothTools exposing (..)

import BendSmoother exposing (SmoothedBend)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


type Msg
    = SmoothBend
    | SetBendTrackPointSpacing Float


type alias BendOptions =
    { smoothedBend : Maybe SmoothedBend
    , bendTrackPointSpacing : Float
    }


defaultOptions =
    { smoothedBend = Nothing
    , bendTrackPointSpacing = 5.0
    }


viewBendFixerPane : BendOptions -> (Msg -> msg) -> Element msg
viewBendFixerPane bendOptions wrap =
    let
        fixBendButton smooth =
            button
                prettyButtonStyles
                { onPress = Just <| wrap SmoothBend
                , label =
                    text <|
                        "Smooth between markers\nRadius "
                            ++ showDecimal2 smooth.radius
                }
    in
    column [ spacing 10, padding 10, alignTop, centerX ]
        [ case bendOptions.smoothedBend of
            Just smooth ->
                row [ spacing 10, padding 10, alignTop ]
                    [ fixBendButton smooth
                    , bendSmoothnessSlider bendOptions wrap
                    ]

            Nothing ->
                column [ spacing 10, padding 10, alignTop, centerX ]
                    [ text "Sorry, failed to find a nice bend."
                    , text "Try re-positioning the current pointer or marker."
                    ]
        ]


bendSmoothnessSlider : BendOptions -> (Msg -> msg) -> Element msg
bendSmoothnessSlider model wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    "Spacing = "
                        ++ showDecimal2 model.bendTrackPointSpacing
        , min = 1.0
        , max = 10.0
        , step = Nothing
        , value = model.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }
