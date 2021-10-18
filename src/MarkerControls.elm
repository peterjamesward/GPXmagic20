module MarkerControls exposing (..)

import ColourPalette exposing (buttonGroupBackground, scrollbarBackground)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import Html.Events.Extra.Mouse as Mouse
import Length
import List.Extra
import PostUpdateActions exposing (PostUpdateAction(..))
import Time
import Track exposing (Track)
import Utils exposing (scrollbarThickness, showDecimal2, showLongMeasure, useIcon)
import ViewPureStyles exposing (conditionallyVisible, defaultColumnLayout, defaultRowLayout, prettyButtonStyles, toolRowLayout)


type Msg
    = ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne
    | PositionForwardOne
    | PositionBackOne
    | PositionSlider Int
    | PositionForwardMouseDown
    | PositionForwardMouseUp
    | PositionTimer
    | PositionBackMouseDown
    | PositionBackMouseUp


type ButtonState
    = Idle
    | ForwardDown Int
    | ReverseDown Int


type alias Options =
    { state : ButtonState
    }


defaultOptions =
    { state = Idle
    }


viewTrackControls : (Msg -> msg) -> Maybe Track -> Element msg
viewTrackControls wrap track =
    Maybe.map (positionControls wrap) track |> Maybe.withDefault none


markerButton : Bool -> Maybe Track -> (Msg -> msg) -> Element msg
markerButton imperial track messageWrapper =
    let
        makeButton label =
            button
                (centerX :: prettyButtonStyles)
                { onPress = Just <| messageWrapper ToggleMarker
                , label =
                    E.text <| label
                }
    in
    case track of
        Nothing ->
            none

        Just isTrack ->
            let
                ( positionText, isDropped ) =
                    case isTrack.markedNode of
                        Just markedTP ->
                            ( wrappedRow [ spacing 5 ]
                                [ text <|
                                    "Orange: "
                                        ++ showLongMeasure imperial isTrack.currentNode.distanceFromStart
                                , text <|
                                    " Purple: "
                                        ++ showLongMeasure imperial markedTP.distanceFromStart
                                ]
                            , True
                            )

                        Nothing ->
                            ( text <|
                                "Orange: "
                                    ++ showLongMeasure imperial isTrack.currentNode.distanceFromStart
                            , False
                            )
            in
            column [ spacing 5, padding 5, alignTop, width fill, centerX ]
                [ row
                    [ spacing 10
                    , padding 10
                    , centerX
                    ]
                    [ conditionallyVisible isDropped <|
                        button
                            prettyButtonStyles
                            { onPress = Just <| messageWrapper MarkerBackOne
                            , label = useIcon FeatherIcons.skipBack
                            }
                    , makeButton <|
                        if isDropped then
                            "Clear marker"

                        else
                            "Drop marker"
                    , conditionallyVisible isDropped <|
                        button
                            prettyButtonStyles
                            { onPress = Just <| messageWrapper MarkerForwardOne
                            , label = useIcon FeatherIcons.skipForward
                            }
                    ]
                , el [ centerX, centerY ] <| positionText
                ]


positionControls : (Msg -> msg) -> Track -> Element msg
positionControls wrap track =
    row
        [ spacing 5
        , padding 5
        , centerX
        , centerY
        ]
        [ positionSlider wrap track
        , el
            ([ htmlAttribute <| Mouse.onDown (always PositionBackMouseDown >> wrap)
             , htmlAttribute <| Mouse.onUp (always PositionBackMouseUp >> wrap)
             ]
                ++ prettyButtonStyles
            )
            (useIcon FeatherIcons.skipBack)
        , el
            ([ htmlAttribute <| Mouse.onDown (always PositionForwardMouseDown >> wrap)
             , htmlAttribute <| Mouse.onUp (always PositionForwardMouseUp >> wrap)
             ]
                ++ prettyButtonStyles
            )
            (useIcon FeatherIcons.skipForward)
        ]


positionSlider : (Msg -> msg) -> Track -> Element msg
positionSlider wrap track =
    Input.slider
        [ height <| px scrollbarThickness
        , width <| px 300
        , centerY
        , behindContent <|
            -- Slider track
            el
                [ width <| px 300
                , height <| px scrollbarThickness
                , centerY
                , centerX
                , Background.color scrollbarBackground
                , Border.rounded 6
                ]
                none
        ]
        { onChange = wrap << (PositionSlider << round)
        , label =
            Input.labelHidden "Drag slider or use arrow buttons"
        , min = 0.0
        , max = toFloat <| List.length track.trackPoints - 1
        , step = Just 1
        , value = toFloat track.currentNode.index
        , thumb = Input.defaultThumb
        }


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction (Cmd msg) )
update msg options wrap track =
    let
        safeNewNode newIndex =
            case List.Extra.getAt newIndex track.trackPoints of
                Just tp ->
                    ActionFocusMove tp

                Nothing ->
                    ActionNoOp
    in
    case ( msg, track.markedNode ) of
        ( PositionForwardOne, _ ) ->
            ( options, safeNewNode <| track.currentNode.index + 1 )

        ( PositionForwardMouseDown, _ ) ->
            ( { options | state = ForwardDown 1 }
            , ActionNoOp
            )

        ( PositionBackMouseDown, _ ) ->
            ( { options | state = ReverseDown 1 }
            , ActionNoOp
            )

        ( PositionTimer, _ ) ->
            case options.state of
                ForwardDown n ->
                    ( { options | state = ForwardDown (min (n + 1) 10) }
                    , safeNewNode <| track.currentNode.index + 1
                    )

                ReverseDown n ->
                    ( { options | state = ReverseDown (min (n + 1) 10) }
                    , safeNewNode <| track.currentNode.index - 1
                    )

                _ ->
                    ( options, ActionNoOp )

        ( PositionForwardMouseUp, _ ) ->
            ( { options | state = Idle }
            , safeNewNode <| track.currentNode.index + 1
            )

        ( PositionBackMouseUp, _ ) ->
            ( { options | state = Idle }
            , safeNewNode <| track.currentNode.index - 1
            )

        ( PositionBackOne, _ ) ->
            ( options, safeNewNode <| track.currentNode.index - 1 )

        ( PositionSlider index, _ ) ->
            ( defaultOptions, safeNewNode index )

        ( ToggleMarker, Just _ ) ->
            ( defaultOptions, ActionMarkerMove Nothing )

        ( ToggleMarker, Nothing ) ->
            ( defaultOptions, ActionMarkerMove <| Just track.currentNode )

        ( MarkerForwardOne, Just mark ) ->
            let
                wrapped =
                    (mark.index + 1) |> modBy (List.length track.trackPoints)
            in
            ( defaultOptions, ActionMarkerMove <| List.Extra.getAt wrapped track.trackPoints )

        ( MarkerBackOne, Just mark ) ->
            let
                wrapped =
                    (mark.index - 1) |> modBy (List.length track.trackPoints)
            in
            ( defaultOptions, ActionMarkerMove <| List.Extra.getAt wrapped track.trackPoints )

        _ ->
            ( defaultOptions, ActionNoOp )


subscription : Options -> (Msg -> msg) -> Sub msg
subscription options wrap =
    case options.state of
        ForwardDown n ->
            Time.every (1000.0 / toFloat n) (always PositionTimer >> wrap)

        ReverseDown n ->
            Time.every (1000.0 / toFloat n) (always PositionTimer >> wrap)

        _ ->
            Sub.none
