module ViewPureStyles exposing (..)

import Color exposing (blue)
import ColourPalette exposing (buttonBackground, buttonShadow, buttonText, collapsedTabBorder, radioButtonDefault, radioButtonSelected, radioButtonText, scrollbarBackground)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (style)


defaultRowLayout =
    []


toolRowLayout =
    [ spacing 10
    , paddingXY 20 10
    , width fill
    ]


defaultColumnLayout =
    [ spacing 5, padding 5, alignTop, width fill ]


prettyButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 10
    , Border.color buttonBackground
    , Background.color buttonBackground
    , Font.color buttonText
    , Font.size 16
    --, mouseOver
    --    [ Background.color buttonText, Font.color buttonBackground ]
    --, focused
    --    [ Border.shadow { offset = ( 4, 0 ), size = 3, blur = 5, color = buttonShadow } ]
    ]


conditionallyVisible : Bool -> Element msg -> Element msg
conditionallyVisible test element =
    -- This turns out to be the secret sauce for easier map integration.
    -- It means we can pre-load a Mapbox map element.
    if test then
        el [] element

    else
        el [ htmlAttribute (style "display" "none") ] element


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


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 32
        , height <| px 32
        , centerY
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color buttonShadow
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    buttonBackground

                else
                    collapsedTabBorder
            ]
        <|
            none


radioButton label state =
    el
        [ padding 10
        , spacing 2
        , Border.widthEach { left = 2, right = 2, top = 2, bottom = 0 }
        , Border.roundEach { topLeft = 10, bottomLeft = 0, topRight = 10, bottomRight = 0 }
        , Background.color <|
            if state == Input.Selected then
                radioButtonSelected

            else
                radioButtonDefault
        , Font.color radioButtonText
        , Font.size 16
        ]
    <|
        el [ centerX, centerY ] <|
            text label


displayName n =
    case n of
        Just s ->
            el [ Font.size 20 ]
                (text s)

        _ ->
            none


wideSliderStyles =
    [ height <| px 20
    , width <| px 360
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 360
            , height <| px 20
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]
