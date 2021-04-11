module ViewPureStyles exposing (..)

import ColourPalette exposing (buttonBackground, buttonShadow, buttonText, scrollbarBackground)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


defaultRowLayout =
    [ spaceEvenly, spacing 10, padding 10 ]


defaultColumnLayout =
    [ spacing 10, padding 10, alignTop ]


prettyButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 16
    , Border.color buttonBackground

    --, Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = rgb255 0xD0 0xD0 0xD0 }
    , Background.color buttonBackground
    , Font.color <| buttonText
    , Font.size 16
    , mouseOver
        [ Background.color buttonText, Font.color buttonBackground ]
    , focused
        [ Border.shadow { offset = ( 4, 0 ), size = 3, blur = 5, color = buttonShadow } ]
    , centerX
    ]


commonShortHorizontalSliderStyles =
    [ height <| px 20
    , width <| px 150
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 20
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


commonShortVerticalSliderStyles =
    [ height <| px 150
    , width <| px 20
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 20
            , height <| px 150
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]
