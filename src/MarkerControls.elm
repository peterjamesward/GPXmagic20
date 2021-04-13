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
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, prettyButtonStyles)


type MarkerControlsMsg
    = ToggleMarker
    | MarkerForwardOne
    | MarkerBackOne


markerButton : Track -> (MarkerControlsMsg -> msg) -> Element msg
markerButton model messageWrapper =
    let
        forward =
            FeatherIcons.skipForward
                |> FeatherIcons.toHtml []

        back =
            FeatherIcons.skipBack
                |> FeatherIcons.toHtml []

        makeButton label =
            button
                prettyButtonStyles
                { onPress = Just <| messageWrapper ToggleMarker
                , label =
                    E.text <| label
                }
    in
    case model.markedNode of
        Just markedTP ->
            column defaultColumnLayout
                [ row defaultRowLayout
                    [ button
                        prettyButtonStyles
                        { onPress = Just <| messageWrapper MarkerBackOne
                        , label = html back
                        }
                    , makeButton "Clear marker"
                    , button
                        prettyButtonStyles
                        { onPress = Just <| messageWrapper MarkerForwardOne
                        , label = html forward
                        }
                    ]
                , el [ centerX, centerY ] <|
                    text <|
                        "Orange: "
                            ++ (showDecimal2 <| Length.inMeters model.currentNode.distanceFromStart)
                            ++ "m. Purple: "
                            ++ (showDecimal2 <| Length.inMeters markedTP.distanceFromStart)
                            ++ "m."
                ]

        Nothing ->
            column defaultColumnLayout
                [ makeButton "Drop marker to select a range"
                , el [ centerX, centerY ] <|
                    text <|
                        "Pointer at "
                            ++ (showDecimal2 <| Length.inMeters model.currentNode.distanceFromStart)
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
