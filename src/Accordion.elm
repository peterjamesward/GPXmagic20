module Accordion exposing (..)

-- Seeking a better way to organise all the controls.

import ColourPalette exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import FlatColors.BritishPalette
import List.Extra
import Markdown
import Scene exposing (Scene)
import Track exposing (Track)
import Url exposing (Url)
import Utils exposing (useIcon)


type AccordionState
    = Expanded Bool
    | Contracted
    | Disabled


type alias AccordionEntry msg =
    --TODO: Use this as dispatcher for tools' updates methods.
    { label : String
    , state : AccordionState
    , content : Element msg
    , info : String
    , video : Maybe String
    , reducedSet : Bool
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
    [ padding 8
    , spacing 2
    , width fill
    , Border.widthEach { left = 2, right = 0, top = 2, bottom = 2 }
    , Border.roundEach { topLeft = 10, bottomLeft = 10, topRight = 0, bottomRight = 0 }
    , Border.color <|
        case state of
            Expanded _ ->
                expandedTabBorder

            _ ->
                collapsedTabBorder
    , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 5, color = expandedTabShadow }
    , Background.color <|
        case state of
            Expanded _ ->
                expandedTabBackground

            _ ->
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
                            Expanded _ ->
                                Contracted

                            Contracted ->
                                Expanded False

                            Disabled ->
                                Disabled
                }

            else
                e
    in
    List.map toggleMatching entries


accordionToggleInfo :
    List (AccordionEntry msg)
    -> String
    -> List (AccordionEntry msg)
accordionToggleInfo entries label =
    let
        toggleMatching e =
            if e.label == label then
                { e
                    | state =
                        case e.state of
                            Expanded info ->
                                Expanded (not info)

                            _ ->
                                e.state
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
    case entry.state of
        Expanded True ->
            button [ onLeft <| viewInfo entry.info ]
                { onPress = Just <| msgWrap (ToggleInfo entry.label)
                , label = useIcon FeatherIcons.info
                }

        Expanded False ->
            button []
                { onPress = Just <| msgWrap (ToggleInfo entry.label)
                , label = useIcon FeatherIcons.info
                }

        _ ->
            none


view :
    List (AccordionEntry msg)
    -> (Msg -> msg)
    -> Element msg
view entries msgWrap =
    let
        viewOpenEntry : AccordionEntry msg -> Element msg
        viewOpenEntry entry =
            row [ width fill ]
                [ infoButton entry msgWrap
                , column [ width fill ]
                    [ row (accordionTabStyles entry.state)
                        [ button [ width fill ]
                            { onPress = Just (msgWrap <| ToggleEntry entry.label)
                            , label = text entry.label
                            }
                        , case entry.video of
                            Just video ->
                                newTabLink
                                    [ alignRight ]
                                    { url = video
                                    , label =
                                        image [ height (px 16) ]
                                            { src = "images/yt_logo_mono_dark.png"
                                            , description = "YouTube"
                                            }
                                    }

                            Nothing ->
                                none
                        ]
                    , el
                        [ Background.color accordionContentBackground
                        , width fill
                        , centerX
                        ]
                        entry.content
                    ]
                ]

        viewClosedEntry : AccordionEntry msg -> Element msg
        viewClosedEntry entry =
            button (accordionTabStyles entry.state)
                { onPress = Just (msgWrap <| ToggleEntry entry.label)
                , label = text entry.label
                }

        isOpen entry =
            entry.state == Expanded True || entry.state == Expanded False

        ( open, closed ) =
            List.partition isOpen entries
    in
    column accordionMenuStyles <|
        List.map viewOpenEntry open
            ++ [ wrappedRow [] (List.map viewClosedEntry closed) ]


update : Msg -> List (AccordionEntry msg) -> List (AccordionEntry msg)
update msg accordion =
    case msg of
        ToggleEntry label ->
            accordionToggle accordion label

        ToggleInfo label ->
            accordionToggleInfo accordion label


viewInfo : String -> Element msg
viewInfo info =
    el [] <|
        row
            [ centerX
            , Background.color <| rgb255 220 220 200
            , clipY
            , scrollbarY
            , padding 20
            , width <| px 500
            , moveUp 200
            , Border.color expandedTabBackground
            , Border.width 3
            ]
            [ paragraph
                [ width fill
                , height <| px 300
                ]
              <|
                [ html <| Markdown.toHtml [] info ]
            ]


tabIsOpen : String -> List (AccordionEntry msg) -> Bool
tabIsOpen label entries =
    entries
        |> List.Extra.find
            (\entry ->
                entry.label
                    == label
                    && (entry.state == Expanded True || entry.state == Expanded False)
            )
        |> (/=) Nothing
