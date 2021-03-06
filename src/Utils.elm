module Utils exposing (..)

import Angle exposing (Angle)
import BoundingBox2d
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Element exposing (..)
import Element.Background as Background
import FeatherIcons
import FlatColors.AmericanPalette
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Http
import Length exposing (Meters)
import LineSegment3d
import List.Extra
import Pixels
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Speed exposing (Speed)
import Vector3d


type alias Point =
    ( Float, Float )


minmax a b =
    ( toFloat <| min a b
    , toFloat <| max a b
    )


type alias DownSelected a =
    { selected : List a
    , countInView : Int
    , countSelected : Int
    }


defaultDownSelected =
    { selected = []
    , countInView = 0
    , countSelected = 0
    }


lollipop pt colour =
    let
        offset =
            if Point3d.zCoordinate pt |> Quantity.lessThan Quantity.zero then
                Vector3d.meters 0 0 -2

            else
                Vector3d.meters 0 0 2

        lollipopAt =
            Point3d.translateBy offset pt
    in
    [ Scene3d.point { radius = Pixels.pixels 12 }
        (Material.color colour)
        pt
    --, Scene3d.lineSegment
    --    (Material.color colour)
    --    (LineSegment3d.from pt lollipopAt)
    ]


reversingCons : List a -> List a -> List a
reversingCons xs ys =
    -- Use this for speed when order can be ignored.
    case ( xs, ys ) of
        ( [], _ ) ->
            ys

        ( _, [] ) ->
            xs

        ( x :: moreX, _ ) ->
            reversingCons moreX (x :: ys)


combineLists : List (List a) -> List a
combineLists lists =
    List.foldl reversingCons [] lists


eyeHeight =
    2.0


subListsBy : (a -> Bool) -> List a -> ( List (List a), List (List a) )
subListsBy predicate items =
    -- Return is (<bits of list where predicate False>, <bits where True>)
    let
        startState =
            { falseSections = []
            , trueSections = []
            }

        consumeFalseSection state source =
            case source |> List.Extra.splitWhen predicate of
                Nothing ->
                    state

                Just ( falseSection, remaining ) ->
                    consumeTrueSection
                        { state | falseSections = falseSection :: state.falseSections }
                        remaining

        consumeTrueSection state source =
            case source |> List.Extra.splitWhen (not << predicate) of
                Nothing ->
                    state

                Just ( trueSection, remaining ) ->
                    consumeFalseSection
                        { state | trueSections = trueSection :: state.trueSections }
                        remaining

        { falseSections, trueSections } =
            consumeFalseSection startState items
    in
    ( List.reverse falseSections, List.reverse trueSections )


subListsByWithSingletons : (a -> Bool) -> List a -> ( List (List a), List a )
subListsByWithSingletons predicate items =
    -- Return is (<bits of list where predicate False>, <singletons where True>)
    let
        startState =
            { falseSections = []
            , trueSections = []
            }

        consumeFalseSection state source =
            case source |> List.Extra.splitWhen predicate of
                Nothing ->
                    { state | falseSections = source :: state.falseSections }

                Just ( falseSection, remaining ) ->
                    consumeTrueSingleton
                        { state | falseSections = falseSection :: state.falseSections }
                        remaining

        consumeTrueSingleton state source =
            case source of
                singleton :: remaining ->
                    consumeFalseSection
                        { state | trueSections = singleton :: state.trueSections }
                        remaining

                _ ->
                    state

        { falseSections, trueSections } =
            consumeFalseSection startState items
    in
    ( List.reverse falseSections, List.reverse trueSections )


subListsByGreedy : (a -> Bool) -> Int -> List a -> ( List (List a), List (List a) )
subListsByGreedy predicate numberToTake items =
    -- Return is (<bits of list where predicate False>, <bits where True>)
    -- but here when we find a True, we consume `numberToTake`, this will reflect
    -- the number of inserted segments.
    -- Return is (<bits of list where predicate False>, <singletons where True>)
    let
        startState =
            { falseSections = []
            , trueSections = []
            }

        consumeFalseSection state source =
            case source |> List.Extra.splitWhen predicate of
                Nothing ->
                    { state | falseSections = source :: state.falseSections }

                Just ( falseSection, remaining ) ->
                    consumeTrueCluster
                        { state | falseSections = falseSection :: state.falseSections }
                        remaining

        consumeTrueCluster state source =
            case source of
                shouldHaveEnough ->
                    consumeFalseSection
                        { state | trueSections = List.take numberToTake shouldHaveEnough :: state.trueSections }
                        (List.drop numberToTake shouldHaveEnough)

        { falseSections, trueSections } =
            consumeFalseSection startState items
    in
    ( List.reverse falseSections, List.reverse trueSections )


elide : List a -> List a
elide input =
    -- Fold is essential  for performance.
    -- Two passes here means we get the list back the right way round.
    let
        helper : List a -> List a -> List a
        helper accum source =
            case source of
                aa :: bb :: cc ->
                    helper (aa :: accum) cc

                [ zz ] ->
                    zz :: accum

                [] ->
                    accum
    in
    input |> helper [] |> helper []


showMaybe : Maybe Int -> String
showMaybe mi =
    case mi of
        Just i ->
            String.fromInt i

        Nothing ->
            "----"


gradientHue : Float -> Float
gradientHue slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue
    in
    x * steepestAscentHue + (1.0 - x) * steepestDescentHue


gradientHue2 : Float -> Float
gradientHue2 slope =
    let
        hueOf col =
            let
                { hue, saturation, lightness, alpha } =
                    Color.toHsla col
            in
            hue
    in
    -- Closer to "standard" colouring.
    if slope < 0 then
        gradientHue slope

    else if slope <= 6.0 then
        interpolate (slope / 6.0) (hueOf Color.lightGreen) (hueOf Color.yellow)

    else if slope <= 9.0 then
        interpolate ((slope - 6.0) / 3.0) (hueOf Color.yellow) (hueOf Color.orange)

    else if slope <= 12.0 then
        interpolate ((slope - 9.0) / 3.0) (hueOf Color.orange) (hueOf Color.red)

    else
        interpolate ((clamp 12 30 slope - 12.0) / 18.0) (hueOf Color.red) (hueOf Color.black)


interpolate x a b =
    x * a + (1.0 - x) * b


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue2 slope) 0.6 0.7


gradientColourVivid : Float -> Color.Color
gradientColourVivid slope =
    Color.hsl (gradientHue2 slope) 1.0 0.4


elmuiColour : Color.Color -> Element.Color
elmuiColour c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    Element.rgb red green blue


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


showAngle : Angle.Angle -> String
showAngle angle =
    showDecimal0 <| Angle.inDegrees angle


showShortMeasure : Bool -> Length.Length -> String
showShortMeasure imperial distance =
    if imperial then
        showDecimal2 (Length.inFeet distance)
            ++ " feet"

    else
        (showDecimal2 <| Length.inMeters distance)
            ++ "m"


showSpeed : Bool -> Speed -> String
showSpeed imperial speed =
    if imperial then
        showDecimal2 (Speed.inMilesPerHour speed)
            ++ "mph"

    else
        showDecimal2 (Speed.inKilometersPerHour speed)
            ++ "kph"


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


showDecimal1 x =
    let
        locale =
            { usLocale
                | decimals = Exact 1
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


flatBox box =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box
    in
    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }


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


clickTolerance =
    ( Length.meters 200.0, Length.meters 200.0 )


withLeadingZeros : Int -> String -> String
withLeadingZeros beforePoint raw =
    String.repeat (beforePoint - String.length raw) "0"
        ++ raw


showLogos : Element msg
showLogos =
    column
        [ padding 20
        , spacing 20
        , moveDown 30
        , moveRight 30
        , alignBottom
        , alignLeft
        , Background.color FlatColors.AmericanPalette.soothingBreeze
        ]
        [ paragraph [] <| [ text "Please use these logos to promote GPXmagic" ]
        , image [ width <| px 200 ]
            { src = "images/smooth-with-gpxmagic-ride-with-rgt.svg"
            , description = "Smooth with GPXmagic"
            }
        , image [ width <| px 200 ] { src = "images/made with black.png", description = "Black logo" }
        , image [ width <| px 200 ] { src = "images/made with purple.png", description = "Purple logo" }
        , image [ width <| px 200 ] { src = "images/made with white.png", description = "White logo" }
        , image [ width <| px 200 ] { src = "images/made with 3 colour.png", description = "3 colour logo" }
        ]
