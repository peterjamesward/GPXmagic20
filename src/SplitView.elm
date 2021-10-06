module SplitView exposing (..)

import Element exposing ( Element, column, el, fill, row, width)
import Html.Attributes as H exposing ( style)
import SplitPane exposing (Msg, Orientation(..), State, ViewConfig, createCustomSplitter)


view : ViewConfig msg -> Element msg -> Element msg -> State -> Element msg
view viewConfig firstView secondView state =
    let
        splitter =
            getConcreteSplitter viewConfig state.orientation state.dragState
    in
    case state.orientation of
        Horizontal ->
            row
                (paneContainerStyle state.orientation
                    ++ [ width fill ]
                )
                [ el (firstChildViewStyle (State state)) firstView
                , splitter
                , el (secondChildViewStyle (State state)) secondView
                ]

        Vertical ->
            column (paneContainerStyle state.orientation)
                [ el (firstChildViewStyle (State state)) firstView
                , splitter
                , el (secondChildViewStyle (State state)) secondView
                ]


viewReversed : ViewConfig msg -> Element msg -> Element msg -> State -> Element msg
viewReversed (ViewConfig viewConfig) firstView secondView (State state) =
    let
        splitter =
            getConcreteSplitter viewConfig state.orientation state.dragState
    in
    case state.orientation of
        Horizontal ->
            row
                (paneContainerStyle state.orientation
                    ++ [ width fill ]
                )
                [ el (secondChildViewStyle (State state)) secondView
                , splitter
                , el (firstChildViewStyle (State state)) firstView
                ]

        Vertical ->
            column (paneContainerStyle state.orientation)
                [ el (secondChildViewStyle (State state)) firstView
                , splitter
                , el (firstChildViewStyle (State state)) firstView
                ]


getConcreteSplitter :
    { toMsg : Msg -> msg
    , splitter : Maybe (CustomSplitter msg)
    }
    -> Orientation
    -> DragState
    -> Element msg
getConcreteSplitter viewConfig orientation4 dragState =
    case viewConfig.splitter of
        Just (CustomSplitter splitter) ->
            splitter

        Nothing ->
            case createCustomSplitter viewConfig.toMsg <| createDefaultSplitterDetails orientation4 dragState of
                CustomSplitter defaultSplitter ->
                    defaultSplitter

paneContainerStyle : Orientation -> List (Attribute msg)
paneContainerStyle orientation5 =
    [ style "overflow" "hidden"
    , style "display" "flex"
    , style "flexDirection"
        (case orientation5 of
            Horizontal ->
                "row"

            Vertical ->
                "column"
        )
    , style "justifyContent" "center"
    , style "alignItems" "center"
    , style "width" "100%"
    , style "height" "100%"
    , style "boxSizing" "border-box"
    ]
        |> List.map Element.htmlAttribute


firstChildViewStyle : State -> List (Attribute msg)
firstChildViewStyle (State state) =
    case state.splitterPosition of
        Px px2 ->
            let
                v =
                    (String.fromFloat <| toFloat (getValue px2)) ++ "px"
            in
            case state.orientation of
                Horizontal ->
                    [ style "display" "flex"
                    , style "width" v
                    , style "height" "100%"
                    , style "overflow" "hidden"
                    , style "boxSizing" "border-box"
                    , style "position" "relative"
                    ]
                        |> List.map Element.htmlAttribute

                Vertical ->
                    [ style "display" "flex"
                    , style "width" "100%"
                    , style "height" v
                    , style "overflow" "hidden"
                    , style "boxSizing" "border-box"
                    , style "position" "relative"
                    ]
                        |> List.map Element.htmlAttribute

        Percentage p ->
            let
                v =
                    String.fromFloat <| getValue p
            in
            [ style "display" "flex"
            , style "flex" v
            , style "width" "100%"
            , style "height" "100%" -- pz edit
            , style "overflow" "hidden"
            , style "boxSizing" "border-box"
            , style "position" "relative"
            ]
                |> List.map Element.htmlAttribute


secondChildViewStyle : State -> List (Attribute msg)
secondChildViewStyle (State state) =
    case state.splitterPosition of
        Px _ ->
            [ style "display" "flex"
            , style "flex" "1"
            , style "width" "100%"
            , style "height" "100%"
            , style "overflow" "hidden"
            , style "boxSizing" "border-box"
            , style "position" "relative"
            ]
                |> List.map Element.htmlAttribute

        Percentage p ->
            let
                v =
                    String.fromFloat <| 1 - getValue p
            in
            [ style "display" "flex"
            , style "flex" v
            , style "width" "100%"
            , style "height" "100%"
            , style "overflow" "hidden"
            , style "boxSizing" "border-box"
            , style "position" "relative"
            ]
                |> List.map Element.htmlAttribute


