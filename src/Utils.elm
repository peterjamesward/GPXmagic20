module Utils exposing (..)

import Angle exposing (Angle, inRadians)
import Color exposing (Color)
import Direction3d
import Element exposing (html)
import FeatherIcons
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Http
import Quantity
import SketchPlane3d


type alias Point =
    ( Float, Float )


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientColourPastel : Float -> Color
gradientColourPastel slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 0.6 0.7


gradientColourVivid : Float -> Color
gradientColourVivid slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue

        hue =
            x * steepestAscentHue + (1.0 - x) * steepestDescentHue
    in
    Color.hsl hue 1.0 0.4


terrainColourFromHeight : Float -> Color
terrainColourFromHeight height =
    let
        x =
            clamp 0.0 3000.0 height / 3000.0

        fullColour = Color.toHsla (Color.rgb255 85 128 0)

        maxHeightSat =
            (Color.toHsla Color.white).saturation

        minHeightSat =
            fullColour.saturation * 0.9

        saturation =
            x * maxHeightSat + (1.0 - x) * minHeightSat
    in
    Color.fromHsla { fullColour | saturation = saturation, lightness = sqrt x }


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


showDecimal0 x =
    let
        locale =
            { usLocale
                | decimals = Exact 0
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


bearingToDisplayDegrees : Maybe Angle -> String
bearingToDisplayDegrees angle =
    case angle of
        Just isAngle ->
            isAngle
                |> Angle.inDegrees
                |> (\x ->
                        if x <= 90 then
                            90 - x

                        else if x > 90 then
                            450 - x

                        else
                            90 + abs x
                   )
                |> showDecimal2

        Nothing ->
            "End"
