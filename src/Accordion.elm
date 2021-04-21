module Accordion exposing (..)

-- Seeking a better way to organise all the controls.

import ColourPalette exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Utils exposing (useIcon)
import ViewPureStyles exposing (prettyButtonStyles)


type AccordionState
    = Expanded
    | Contracted
    | Disabled


type alias AccordionEntry msg =
    { label : String
    , state : AccordionState
    , content : Element msg
    , info : String
    }


type Msg
    = ToggleEntry String
    | ToggleInfo String


accordionMenuStyles =
    [ padding 0
    , alignTop
    , width fill
    ]


accordionTabStyles state =
    [ padding 10
    , spacing 2
    , width fill
    , Border.widthEach { left = 2, right = 0, top = 2, bottom = 2 }
    , Border.roundEach { topLeft = 10, bottomLeft = 10, topRight = 0, bottomRight = 0 }
    , Border.color <|
        if state == Expanded then
            expandedTabBorder

        else
            collapsedTabBorder
    , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = expandedTabShadow }
    , Background.color <|
        if state == Expanded then
            expandedTabBackground

        else
            collapsedTabBackground
    , Font.color buttonText
    , Font.center
    , Font.size 16
    ]


accordionToggle :
    List (AccordionEntry msg)
    -> String
    -> List (AccordionEntry msg)
accordionToggle entries label =
    let
        toggleMatching e =
            if e.label == label then
                { e
                    | state =
                        case e.state of
                            Expanded ->
                                Contracted

                            Contracted ->
                                Expanded

                            Disabled ->
                                Disabled
                }

            else
                e
    in
    List.map toggleMatching entries


infoButton :
    AccordionEntry msg
    -> (Msg -> msg)
    -> Element msg
infoButton entry msgWrap =
    if entry.state == Expanded then
        button []
            { onPress = Just <| msgWrap (ToggleInfo entry.label)
            , label = useIcon FeatherIcons.info
            }

    else
        none


accordionView :
    List (AccordionEntry msg)
    -> (Msg -> msg)
    -> Element msg
accordionView entries msgWrap =
    let
        viewEntry : AccordionEntry msg -> Element msg
        viewEntry entry =
            row [ width fill ]
                [ infoButton entry msgWrap
                , column [ width fill ]
                    [ button (accordionTabStyles entry.state)
                        { onPress = Just (msgWrap <| ToggleEntry entry.label)
                        , label = text entry.label
                        }
                    , if entry.state == Expanded then
                        el
                            [ Background.color accordionContentBackground
                            , width fill
                            , centerX
                            ]
                            entry.content

                      else
                        none
                    ]
                ]
    in
    column accordionMenuStyles (List.map viewEntry entries)


accordionActiveItem : List (AccordionEntry msg) -> Maybe (AccordionEntry msg)
accordionActiveItem entries =
    List.head <| List.filter (\e -> e.state == Expanded) entries


update : Msg -> List (AccordionEntry msg) -> List (AccordionEntry msg)
update msg accordion =
    case msg of
        ToggleEntry label ->
            accordionToggle accordion label

        ToggleInfo entry ->
            accordion
