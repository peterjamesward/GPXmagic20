module TrackSplitter exposing (..)

import Delay
import Element exposing (..)
import Element.Input as Input exposing (button)
import File.Download
import Length
import List.Extra
import Quantity
import Track exposing (Track)
import TrackObservations
import Utils exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (prettyButtonStyles, wideSliderStyles)
import WriteGPX


info =
    """## Track splitter

"Track splitter" can be used to deal with really long routes, such as you may encounter
if you tried to replicate a big Audax adventure. We know Magic Roads limits us to 100km, but you can set your
own limit on the length, and this will work out roughly equal splits within the limit you set. It writes them
out as new files in your Downloads folder but does not replace the loaded track.
"""


type Msg
    = SplitTrack
    | SetSplitLimit Int
    | WriteSection (List ( Float, Float ))


type alias Options =
    { splitLimit : Int
    }


defaultOptions : Options
defaultOptions =
    { splitLimit = 100
    }


update :
    Msg
    -> Options
    -> TrackObservations.TrackObservations
    -> Maybe Track
    -> ( Options, Cmd Msg )
update msg settings observations mTrack =
    case msg of
        SetSplitLimit n ->
            ( { settings
                | splitLimit = n
              }
            , Cmd.none
            )

        SplitTrack ->
            case mTrack of
                Just track ->
                    ( settings
                    , Cmd.batch
                        [ Delay.after 100 <|
                            WriteSection <|
                                writeSections
                                    track
                                    observations.trackLength
                                    settings.splitLimit
                        ]
                    )

                Nothing ->
                    ( settings, Cmd.none )

        WriteSection sections ->
            case ( mTrack, sections ) of
                ( Just track, ( start, end ) :: rest ) ->
                    let
                        metricStart =
                            Length.meters <| start * 1000.0

                        metricEnd =
                            Length.meters <| end * 1000.0

                        trackName =
                            track.trackName |> Maybe.withDefault "track"

                        filename =
                            trackName
                                ++ "_"
                                ++ showDecimal0 start
                                ++ "_"
                                ++ showDecimal0 end
                                ++ ".gpx"

                        trackExtract =
                            { track
                                | trackPoints =
                                    track.trackPoints
                                        |> List.Extra.dropWhile
                                            (.distanceFromStart >> Quantity.lessThan metricStart)
                                        |> List.Extra.takeWhile
                                            (.distanceFromStart >> Quantity.lessThanOrEqualTo metricEnd)
                            }

                        content =
                            WriteGPX.writeGPX trackExtract
                    in
                    ( settings
                    , Cmd.batch
                        [ File.Download.string filename "text/xml" content
                        , Delay.after 1000 <| WriteSection rest
                        ]
                    )

                _ ->
                    ( settings, Cmd.none )


writeSections : Track -> Float -> Int -> List ( Float, Float )
writeSections track length limit =
    -- Doesn't *actually* split the track, just writes out the files.
    -- This function works out where the splits are, then each section is
    -- written out using the runtime, which kicks off the next.
    let
        splitCount =
            ceiling (length / (1000.0 * toFloat limit))

        splitLength =
            length / toFloat splitCount / 1000.0

        splitPoints =
            List.map (toFloat >> (*) splitLength) (List.range 0 splitCount)
    in
    List.map2 Tuple.pair splitPoints (List.drop 1 splitPoints)


view : Options -> TrackObservations.TrackObservations -> (Msg -> msg) -> Track -> Element msg
view options observations wrapper track =
    let
        splitCount =
            ceiling (observations.trackLength / (1000.0 * toFloat options.splitLimit))

        splitLength =
            observations.trackLength / toFloat splitCount / 1000.0

        partsSlider =
            Input.slider
                wideSliderStyles
                { onChange = wrapper << SetSplitLimit << round
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Max length: "
                                ++ String.fromInt options.splitLimit
                                ++ "km"
                , min = 20
                , max = 100
                , step = Just 5.0
                , value = toFloat options.splitLimit
                , thumb = Input.defaultThumb
                }
    in
    column [ spacing 5, padding 5, centerX ]
        [ paragraph [ centerX, width fill, paddingXY 20 0 ]
            [ text "Files will be written to Downloads "
            , text "folder at one second intervals."
            ]
        , partsSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| SplitTrack
            , label =
                text <|
                    "Split into "
                        ++ String.fromInt splitCount
                        ++ " files\n"
                        ++ "each "
                        ++ showDecimal2 splitLength
                        ++ "km long."
            }
        ]
