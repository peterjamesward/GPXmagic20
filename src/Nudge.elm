module Nudge exposing (..)

import Angle
import Element exposing (Element, centerX, column, el, px, row, text, width)
import Element.Input as Input exposing (button)
import Length exposing (Length)
import Point2d
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal2)
import Vector2d
import ViewPureStyles exposing (..)


type NudgeMsg
    = SetHorizontalNudgeFactor Length
    | SetVerticalNudgeFactor Length
    | NudgeNode NudgeSettings


type alias NudgeSettings =
    { horizontal : Length
    , vertical : Length
    }


defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    }



--nudgeTrackPoint : TrackPoint -> Float -> Float -> TrackPoint
--nudgeTrackPoint baseTP horizontal vertical =
--    let
--        roadVector =
--            -- The negation because, no idea.
--            Vector2d.rTheta (Length.meters 1.0)
--                (Angle.radians <| -1.0 * baseTP.naturalBearing)
--                |> Vector2d.rotateClockwise
--
--        nudgeVector =
--            Vector2d.perpendicularTo roadVector
--                |> Vector2d.scaleBy (horizontal / metresPerDegree)
--
--        trackPoint2d =
--            Point2d.meters baseTP.lon baseTP.lat
--
--        nudgedTrackPoint2d =
--            Point2d.translateBy nudgeVector trackPoint2d
--
--        ( lon, lat ) =
--            Point2d.toTuple Length.inMeters nudgedTrackPoint2d
--    in
--    { baseTP
--        | lat = lat
--        , lon = lon
--        , ele = baseTP.ele + vertical
--        , xyz = fromGPXcoords lon lat (baseTP.ele + vertical)
--    }
--


viewNudgeTools : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
viewNudgeTools settings msgWrapper =
    column defaultColumnLayout
        [ row defaultRowLayout
            [ verticalNudgeSlider settings.vertical msgWrapper
            , column defaultColumnLayout
                [ horizontalNudgeSlider settings.horizontal msgWrapper
                , nudgeButton settings msgWrapper
                ]
            ]
        ]


horizontalNudgeSlider : Length -> (NudgeMsg -> msg) -> Element msg
horizontalNudgeSlider value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetHorizontalNudgeFactor >> wrap
        , label =
            Input.labelBelow [] <|
                text <|
                    "Offset = "
                        ++ (showDecimal2 <| Length.inMeters value)
                        ++ "m"
        , min = -5.0
        , max = 5.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


verticalNudgeSlider : Length -> (NudgeMsg -> msg) -> Element msg
verticalNudgeSlider value wrap =
    el [ width <| px 80, centerX ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = Length.meters >> SetVerticalNudgeFactor >> wrap
            , label =
                Input.labelBelow [ centerX ] <|
                    text <|
                        "Height = "
                            ++ (showDecimal2 <| Length.inMeters value)
                            ++ "m"
            , min = -5.0
            , max = 5.0
            , step = Nothing
            , value = Length.inMeters value
            , thumb = Input.defaultThumb
            }


nudgeButton : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
nudgeButton settings wrap =
    button
        prettyButtonStyles
        { onPress = Just <| wrap (NudgeNode settings)
        , label = text "Apply nudge"
        }


update : NudgeMsg -> NudgeSettings -> Track -> ( NudgeSettings, Track )
update msg settings track =
    case msg of
        SetHorizontalNudgeFactor length ->
            ( { settings | horizontal = length }, track )

        SetVerticalNudgeFactor length ->
            ( { settings | vertical = length }, track )

        NudgeNode _ ->
            ( settings, track )
