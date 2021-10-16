module Utils exposing (..)

import Angle exposing (Angle)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Element exposing (..)
import FeatherIcons
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Http
import Length exposing (Meters)
import Quantity exposing (Quantity)


type alias Point =
    ( Float, Float )


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientColourPastel : Float -> Color.Color
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


gradientColourVivid : Float -> Color.Color
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


terrainColourFromHeight : Float -> Color.Color
terrainColourFromHeight height =
    let
        lightness =
            clamp 0.1 0.9 <| sqrt <| sqrt <| abs <| height / 3000.0
    in
    Color.hsl
        ((80.0 + 50.0 * sin height) / 255)
        (133 / 255)
        lightness


scrollbarThickness =
    20


showLongMeasure : Bool -> Length.Length -> String
showLongMeasure imperial distance =
    if imperial then
        showDecimal2 (Length.inMiles distance)
            ++ " miles"

    else
        (showDecimal2 <| Length.inMeters distance)
            ++ "m"

showShortMeasure : Bool -> Length.Length -> String
showShortMeasure imperial distance =
    if imperial then
        showDecimal2 (Length.inFeet distance)
            ++ " feet"

    else
        (showDecimal2 <| Length.inMeters distance)
            ++ "m"


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
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize 20


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


correlation : (a -> Float) -> (a -> Float) -> List a -> Float
correlation xFn yFn records =
    let
        n =
            List.length records |> toFloat

        squared x =
            x * x

        xy rec =
            xFn rec * yFn rec

        sumXY =
            records |> List.map xy |> List.sum

        sumX =
            records |> List.map xFn |> List.sum

        sumY =
            records |> List.map yFn |> List.sum

        sumXX =
            records |> List.map (xFn >> squared) |> List.sum

        sumYY =
            records |> List.map (yFn >> squared) |> List.sum
    in
    (n * sumXY - sumX * sumY) / (sqrt (n * sumXX - squared sumX) * (n * sumYY - squared sumY))


squareAspect : BoundingBox3d a b -> BoundingBox3d a b
squareAspect box =
    let
        centre =
            BoundingBox3d.centerPoint box

        ( sizeX, sizeY, sizeZ ) =
            BoundingBox3d.dimensions box
    in
    BoundingBox3d.withDimensions
        ( Quantity.max sizeX sizeY
        , Quantity.max sizeX sizeY
        , sizeZ
        )
        centre


showLabelledValues pairs =
    let
        showLabel label =
            text label

        showValue value =
            el [ alignRight ] <| text value
    in
    row [ spacing 5, padding 5 ]
        [ column [ spacing 5 ] <| List.map (Tuple.first >> showLabel) pairs
        , column [ spacing 5 ] <| List.map (Tuple.second >> showValue) pairs
        ]


withLeadingZeros : Int -> String -> String
withLeadingZeros beforePoint raw =
    String.repeat (beforePoint - String.length raw) "0"
        ++ raw
