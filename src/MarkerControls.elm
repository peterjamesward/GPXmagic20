module MarkerControls exposing (..)

import ColourPalette exposing (buttonGroupBackground)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import List.Extra
import Track exposing (Track)
import ViewPureStyles exposing (prettyButtonStyles)


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
    row
        [ padding 5
        , spacing 10
        , Border.width 0
        , Border.rounded 5
        , width fill
        , centerX
        , Font.size 16
        , Background.color buttonGroupBackground
        ]
    <|
        case model.markedNode of
            Just _ ->
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

            Nothing ->
                [ makeButton "Drop marker to select a range" ]


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
