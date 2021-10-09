module MarkerControls exposing (..)

import ColourPalette exposing (buttonGroupBackground, scrollbarBackground)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import Length
import List.Extra
import PostUpdateActions exposing (PostUpdateAction(..))
import Track exposing (Track)
import Utils exposing (scrollbarThickness, showDecimal2, useIcon)
import ViewPureStyles exposing (conditionallyVisible, defaultColumnLayout, defaultRowLayout, prettyButtonStyles, toolRowLayout)


type Msg
    = ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne
    | PositionForwardOne
    | PositionBackOne
    | PositionSlider Int


viewTrackControls : (Msg -> msg) -> Maybe Track -> Element msg
viewTrackControls wrap track =
    Maybe.map (positionControls wrap) track |> Maybe.withDefault none


markerButton : Maybe Track -> (Msg -> msg) -> Element msg
markerButton track messageWrapper =
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
                            ( text <|
                                "Orange: "
                                    ++ (showDecimal2 <| Length.inMeters isTrack.currentNode.distanceFromStart)
                                    ++ "m. Purple: "
                                    ++ (showDecimal2 <| Length.inMeters markedTP.distanceFromStart)
                                    ++ "m."
                            , True
                            )

                        Nothing ->
                            ( text <|
                                "Orange: "
                                    ++ (showDecimal2 <| Length.inMeters isTrack.currentNode.distanceFromStart)
                                    ++ "m."
                            , False
                            )
            in
            column [ spacing 5, padding 5, alignTop, width fill, centerX ]
                [ row
                    [ spacing 10
                    , padding 10
                    , centerX
                    ]
                    [ if isDropped then
                        none

                      else
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
                    , if isDropped then
                        none

                      else
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
        , button
            prettyButtonStyles
            { onPress = Just <| wrap PositionBackOne
            , label = useIcon FeatherIcons.skipBack
            }
        , button
            prettyButtonStyles
            { onPress = Just <| wrap PositionForwardOne
            , label = useIcon FeatherIcons.skipForward
            }
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


update : Msg -> Track -> PostUpdateActions.PostUpdateAction msg
update msg track =
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
            safeNewNode <| track.currentNode.index + 1

        ( PositionBackOne, _ ) ->
            safeNewNode <| track.currentNode.index - 1

        ( PositionSlider index, _ ) ->
            safeNewNode index

        ( ToggleMarker, Just _ ) ->
            ActionMarkerMove Nothing

        ( ToggleMarker, Nothing ) ->
            ActionMarkerMove <| Just track.currentNode

        ( MarkerForwardOne, Just mark ) ->
            let
                wrapped =
                    (mark.index + 1) |> modBy (List.length track.trackPoints)
            in
            ActionMarkerMove <| List.Extra.getAt wrapped track.trackPoints

        ( MarkerBackOne, Just mark ) ->
            let
                wrapped =
                    (mark.index - 1) |> modBy (List.length track.trackPoints)
            in
            ActionMarkerMove <| List.Extra.getAt wrapped track.trackPoints

        _ ->
            ActionNoOp
