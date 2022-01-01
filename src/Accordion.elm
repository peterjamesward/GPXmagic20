module Accordion exposing (..)

-- Seeking a better way to organise all the controls.

import Color
import ColourPalette exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import FlatColors.BritishPalette
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import Json.Decode as D exposing (Error, decodeValue, field)
import Json.Encode as E
import List.Extra
import LocalCoords exposing (LocalCoords)
import Markdown
import Scene exposing (Scene)
import Track exposing (Track)
import Utils exposing (useIcon)


type AccordionState
    = Expanded Bool
    | Contracted
    | Disabled


type alias AccordionEntry mainModel msg =
    { label : String
    , state : AccordionState
    , content : mainModel -> Element msg
    , info : String
    , video : Maybe String
    , isFavourite : Bool
    , preview3D : Maybe (Track -> Scene)
    , previewProfile : Maybe (Track -> Scene)
    , previewMap : Maybe (Track -> E.Value)
    , colour : Color
    }


type alias AccordionStoredState =
    { model : AccordionModel
    , tabs : List ToolStoredState
    }


type alias ToolStoredState =
    { label : String
    , state : String
    , isFavourite : Bool
    }


type alias AccordionModel =
    { reducedToolset : Bool
    }


defaultState : AccordionModel
defaultState =
    { reducedToolset = False }


type Msg
    = ToggleEntry String
    | ToggleInfo String
    | ToggleFavourite String
    | ToggleToolSet
    | RestoreDefaultTools


accordionMenuStyles =
    [ padding 0
    , alignTop
    , width fill
    ]


accordionTabStyles state colour =
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
                colour

            _ ->
                collapsedTabBackground
    , Font.color <|
        case state of
            Expanded _ ->
                FlatColors.FlatUIPalette.midnightBlue

            _ ->
                FlatColors.FlatUIPalette.clouds
    , Font.center
    , Font.size 16
    , case state of
            Expanded _ ->
                Font.bold

            _ ->
                Font.medium
    ]

defaultTabColour = FlatColors.FlatUIPalette.emerald

storedState : AccordionModel -> List (AccordionEntry model msg) -> E.Value
storedState state entries =
    let
        encodedModel =
            E.object [ ( "reducedToolset", E.bool state.reducedToolset ) ]

        encodedTab : AccordionEntry model msg -> E.Value
        encodedTab tab =
            E.object
                [ ( "label", E.string tab.label )
                , ( "state", E.string <| encodeState tab.state )
                , ( "isFavourite", E.bool tab.isFavourite )
                ]

        encodeState s =
            case s of
                Expanded _ ->
                    "Expanded"

                Contracted ->
                    "Contracted"

                Disabled ->
                    "Disabled"
    in
    E.object
        [ ( "model", encodedModel )
        , ( "tabs", E.list encodedTab entries )
        ]


recoverStoredState : E.Value -> List (AccordionEntry model msg) -> ( AccordionModel, List (AccordionEntry model msg) )
recoverStoredState savedState accordion =
    let
        fromStorage =
            D.decodeValue storedStateDecoder savedState

        withSavedValues : List ToolStoredState -> List (AccordionEntry model msg) -> List (AccordionEntry model msg)
        withSavedValues stored original =
            List.map (applyStoredSettings stored) original

        applyStoredSettings : List ToolStoredState -> AccordionEntry model msg -> AccordionEntry model msg
        applyStoredSettings stored entry =
            let
                relevantStoredEntry : Maybe ToolStoredState
                relevantStoredEntry =
                    List.Extra.find (\e -> e.label == entry.label) stored
            in
            case relevantStoredEntry of
                Just storedEntry ->
                    { entry
                        | state = decodeStoredState storedEntry.state
                        , isFavourite = storedEntry.isFavourite
                    }

                Nothing ->
                    entry

        decodeStoredState s =
            case s of
                "Expanded" ->
                    Expanded False

                "Contracted" ->
                    Contracted

                _ ->
                    Disabled
    in
    case fromStorage of
        Ok recoveredState ->
            ( recoveredState.model
            , withSavedValues recoveredState.tabs accordion
            )

        _ ->
            ( defaultState, accordion )


storedStateDecoder : D.Decoder AccordionStoredState
storedStateDecoder =
    D.map2 AccordionStoredState
        (field "model" modelDecoder)
        (field "tabs" (D.list tabDecoder))


modelDecoder : D.Decoder AccordionModel
modelDecoder =
    D.map AccordionModel
        (field "reducedToolset" D.bool)


tabDecoder : D.Decoder ToolStoredState
tabDecoder =
    D.map3 ToolStoredState
        (field "label" D.string)
        (field "state" D.string)
        (field "isFavourite" D.bool)


accordionToggle :
    List (AccordionEntry model msg)
    -> String
    -> List (AccordionEntry model msg)
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
    List (AccordionEntry model msg)
    -> String
    -> List (AccordionEntry model msg)
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


accordionToggleFavourite :
    List (AccordionEntry model msg)
    -> String
    -> List (AccordionEntry model msg)
accordionToggleFavourite entries label =
    let
        toggleMatching e =
            if e.label == label then
                { e | isFavourite = not e.isFavourite }

            else
                e
    in
    List.map toggleMatching entries


infoButton :
    AccordionEntry model msg
    -> (Msg -> msg)
    -> Element msg
infoButton entry msgWrap =
    case entry.state of
        Expanded True ->
            button [ onRight <| viewInfo entry.info ]
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
    AccordionModel
    -> List (AccordionEntry mainModel msg)
    -> (Msg -> msg)
    -> mainModel
    -> Element msg
view accordion entries msgWrap mainModel =
    let
        favouriteButton entry =
            button
                [ Font.color <|
                    if entry.isFavourite then
                        buttonBackground

                    else
                        buttonText
                ]
                { onPress = Just <| msgWrap (ToggleFavourite entry.label)
                , label = useIcon FeatherIcons.star
                }

        viewOpenEntry : AccordionEntry mainModel msg -> Element msg
        viewOpenEntry entry =
            row [ width fill ]
                [ infoButton entry msgWrap
                , column [ width fill ]
                    [ row (accordionTabStyles entry.state entry.colour)
                        [ favouriteButton entry
                        , button [ width fill ]
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
                        (entry.content mainModel)
                    ]
                ]

        viewClosedEntry : AccordionEntry mainModel msg -> Element msg
        viewClosedEntry entry =
            button (accordionTabStyles entry.state entry.colour)
                { onPress = Just (msgWrap <| ToggleEntry entry.label)
                , label = text entry.label
                }

        viewAnyEntry : AccordionEntry mainModel msg -> Element msg
        viewAnyEntry e =
            if isOpen e then
                viewOpenEntry e

            else
                viewClosedEntry e

        isOpen entry =
            entry.state == Expanded True || entry.state == Expanded False

        ( starred, unstarred ) =
            List.partition .isFavourite entries

        ( openStarred, closedStarred ) =
            List.partition isOpen starred

        ( openOther, closedOther ) =
            List.partition isOpen unstarred

        ( allOpen, allClosed ) =
            List.partition isOpen entries

        showHideUnstarred =
            button
                [ Border.width 2
                , Border.color FlatColors.BritishPalette.nanohanachaGold
                , padding 5
                , Border.rounded 3
                ]
                { onPress = Just <| msgWrap ToggleToolSet
                , label =
                    if accordion.reducedToolset then
                        text "Show all"

                    else
                        text "Show Starred only"
                }

        resetTools =
            button
                [ Border.width 2
                , Border.color FlatColors.BritishPalette.nanohanachaGold
                , padding 5
                , Border.rounded 3
                , alignRight
                ]
                { onPress = Just <| msgWrap RestoreDefaultTools
                , label = text "Restore to defaults"
                }
    in
    column accordionMenuStyles
        [ row [ width fill ] [ showHideUnstarred, resetTools ]
        , column [] <| List.map viewOpenEntry allOpen
        , if accordion.reducedToolset then
            wrappedRow [] <| List.map viewClosedEntry closedStarred

          else
            wrappedRow [] <| List.map viewClosedEntry allClosed
        ]


update : Msg -> AccordionModel -> List (AccordionEntry model msg) -> ( AccordionModel, List (AccordionEntry model msg) )
update msg model accordion =
    let
        closeAndDefavourite entry =
            { entry | state = Contracted, isFavourite = False }
    in
    case msg of
        ToggleEntry label ->
            ( model, accordionToggle accordion label )

        ToggleInfo label ->
            ( model, accordionToggleInfo accordion label )

        ToggleFavourite label ->
            ( model, accordionToggleFavourite accordion label )

        ToggleToolSet ->
            ( { model | reducedToolset = not model.reducedToolset }
            , accordion
            )

        RestoreDefaultTools ->
            ( defaultState
            , List.map closeAndDefavourite accordion
            )


viewInfo : String -> Element msg
viewInfo info =
    el [] <|
        row
            [ centerX
            , Background.color <| rgb255 220 220 200
            , clipY
            , scrollbars
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


tabIsOpen : String -> List (AccordionEntry model msg) -> Bool
tabIsOpen label entries =
    entries
        |> List.Extra.find
            (\entry ->
                entry.label
                    == label
                    && (entry.state == Expanded True || entry.state == Expanded False)
            )
        |> (/=) Nothing


openTabs : List (AccordionEntry model msg) -> List (AccordionEntry model msg)
openTabs tabs =
    tabs
        |> List.filter
            (\tab ->
                case tab.state of
                    Expanded _ ->
                        True

                    _ ->
                        False
            )

closedTabs : List (AccordionEntry model msg) -> List (AccordionEntry model msg)
closedTabs tabs =
    tabs
        |> List.filter
            (\tab ->
                case tab.state of
                    Expanded _ ->
                        False

                    _ ->
                        True
            )
