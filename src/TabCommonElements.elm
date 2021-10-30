module TabCommonElements exposing (..)

import ColourPalette exposing (warningColor)
import Element exposing (..)
import Element.Background as Background
import FeatherIcons
import Track exposing (Track)
import Utils exposing (useIcon)


wholeTrackTextHelper : Track -> Element msg
wholeTrackTextHelper track =
    let
        wholeTrack =
            case track.markedNode of
                Just m ->
                    m == track.currentNode

                Nothing ->
                    True
    in
    row [ padding 5, spacing 10, Background.color warningColor, width fill ]
        [ useIcon FeatherIcons.info
        , case wholeTrack of
            True ->
                paragraph []
                    [ text "Applies to the whole route but not the start/finish."
                    , text "Position markers to apply only to a range of points."
                    , text "Convert the track to a loop to smooth the start/finish."
                    ]

            False ->
                paragraph []
                    [ text "Applies between the marker cones only."
                    , text "Clear the marker to apply to the whole route."
                    ]
        ]


markerTextHelper : Track -> Element msg
markerTextHelper track =
    case track.markedNode of
        Just _ ->
            none

        Nothing ->
            row [ padding 5, spacing 10, Background.color warningColor, width fill ]
                [ useIcon FeatherIcons.info
                , paragraph []
                    [ text "This tool needs the Purple marker."
                    ]
                ]
