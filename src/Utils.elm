module Utils exposing (..)

import Angle exposing (Angle, inRadians)
import Color exposing (Color)
import Element exposing (html)
import FeatherIcons
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Http


type alias Point =
    ( Float, Float )


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientColourPastel : Angle -> Color
gradientColourPastel angle =
    --TODO: Unify these two very similar functions.
    let
        slope =
            100.0 * Angle.tan angle

        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 0.6 0.5


gradientColourVivid : Angle -> Color
gradientColourVivid angle =
    let
        slope =
            100.0 * Angle.tan angle

        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 0.9 0.6


scrollbarThickness =
    20


showDecimal2 x =
    let
        locale =
            { usLocale
                | decimals = Exact 2
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


showDecimal6 x =
    let
        locale =
            { usLocale
                | decimals = Exact 6
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


showList : List Int -> String
showList ints =
    String.join ", " <|
        List.map
            String.fromInt
            ints


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


useIcon =
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize 24
