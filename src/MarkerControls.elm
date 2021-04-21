module MarkerControls exposing (..)

import ColourPalette exposing (buttonGroupBackground)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Length
import List.Extra
import Track exposing (Track)
import Utils exposing (showDecimal2, useIcon)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, prettyButtonStyles, toolRowLayout)


type MarkerControlsMsg
    = ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne


markerButton : Maybe Track -> (MarkerControlsMsg -> msg) -> Element msg
markerButton track messageWrapper =
    let
        makeButton label =
            button
                prettyButtonStyles
                { onPress = Just <| messageWrapper ToggleMarker
                , label =
                    E.text <| label
                }
    in
    case track of
        Nothing ->
            none

        Just isTrack ->
            case isTrack.markedNode of
                Just markedTP ->
                    column defaultColumnLayout
                        [ row toolRowLayout
                            [ button
                                prettyButtonStyles
                                { onPress = Just <| messageWrapper MarkerBackOne
                                , label = useIcon FeatherIcons.skipBack
                                }
                            , makeButton "Clear marker"
                            , button
                                prettyButtonStyles
                                { onPress = Just <| messageWrapper MarkerForwardOne
                                , label = useIcon FeatherIcons.skipForward
                                }
                            ]
                        , el [ centerX, centerY ] <|
                            text <|
                                "Orange: "
                                    ++ (showDecimal2 <| Length.inMeters isTrack.currentNode.distanceFromStart)
                                    ++ "m. Purple: "
                                    ++ (showDecimal2 <| Length.inMeters markedTP.distanceFromStart)
                                    ++ "m."
                        ]

                Nothing ->
                    column defaultColumnLayout
                        [ makeButton "Drop marker to select a range"
                        , el [ centerX, centerY] <|
                            text <|
                                "Pointer at "
                                    ++ (showDecimal2 <| Length.inMeters isTrack.currentNode.distanceFromStart)
                                    ++ "m"
                        ]


update : MarkerControlsMsg -> Track -> Track
update msg track =
    case ( msg, track.markedNode ) of
        ( ToggleMarker, Just _ ) ->
            { track | markedNode = Nothing }

        ( ToggleMarker, Nothing ) ->
            { track | markedNode = Just track.currentNode }

        ( MarkerForwardOne, Just mark ) ->
            let
                wrapped =
                    (mark.index + 1) |> modBy (List.length track.track)
            in
            { track | markedNode = List.Extra.getAt wrapped track.track }

        ( MarkerBackOne, Just mark ) ->
            let
                wrapped =
                    (mark.index - 1) |> modBy (List.length track.track)
            in
            { track | markedNode = List.Extra.getAt wrapped track.track }

        _ ->
            track
