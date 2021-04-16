module Utils exposing (..)

import Angle exposing (Angle, inRadians)
import Color exposing (Color)
import Element exposing (html)
import FeatherIcons
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Http
import Pixels
import Regex


type alias Point =
    ( Float, Float )


toDegrees rads =
    rads * 180.0 / pi


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientColourVivid slope =
    -- Note we want (say) 15% to be maximum Red, flat is Green, -15% purple.
    let
        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 1.0 0.4


gradientColourPastel : Angle -> Color
gradientColourPastel angle =
    let
        slope =
            tan (inRadians angle) * 100.0

        x =
            (clamp -15.0 15.0 slope + 15.0) / 30.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 0.6 0.7


asRegex t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


view3dDimensions =
    ( Pixels.int view3dWidth, Pixels.int view3dHeight )


viewMapDimensions =
    ( Pixels.int viewMapWidth, Pixels.int viewMapHeight )


viewMapWidth =
    900


viewMapHeight =
    600


scrollbarThickness =
    20


scrollbarSpaceOcuppied =
    scrollbarThickness + 20



-- allow for 10 padding each side


view3dWidth =
    viewMapWidth - scrollbarSpaceOcuppied


view3dHeight =
    viewMapHeight - scrollbarThickness


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
