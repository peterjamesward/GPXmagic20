module TrackSplitter exposing (..)

import Angle exposing (Angle)
import Axis3d
import ColourPalette exposing (scrollbarBackground)
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import File.Download as Download
import Length exposing (Meters, inMeters, meters)
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Point3d
import PostUpdateActions
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackObservations
import TrackPoint exposing (TrackPoint, prepareTrackPoints, trackPointFromPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles, wideSliderStyles)
import WriteGPX


info =
    """## Track splitter

Divide track into smaller tracks, each in its own file.
"""


type Msg
    = SplitTrack
    | SetSplitLimit Int
    | WriteSection (List ( Float, Float ))


type alias Options =
    { splitLimit : Int
    , splitCount : Int
    , splitLength : Float
    }


defaultOptions : Options
defaultOptions =
    { splitLimit = 100
    , splitCount = 1
    , splitLength = 100.0
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
            let
                splitCount =
                    ceiling (observations.trackLength / (1000.0 * toFloat settings.splitLimit))

                splitLength =
                    observations.trackLength / toFloat splitCount / 1000.0
            in
            ( { settings
                | splitLimit = n
                , splitCount = splitCount
                , splitLength = splitLength
              }
            , Cmd.none
            )

        SplitTrack ->
            case mTrack of
                Just track ->
                    ( settings, writeSections track settings )

                Nothing ->
                    ( settings, Cmd.none )

        WriteSection sections ->
            ( settings, Cmd.none )


writeSections : Track -> Options -> Cmd msg
writeSections track options =
    -- Doesn't *actually* split the track, just writes out the files.
    -- This function works out where the splits are, then each section is
    -- written out using the runtime, which kicks off the next.
    let
        splitPoints =
            List.map (toFloat >> (*) options.splitLength) (List.range 0 options.splitCount)

        trackSections =
            List.map2 Tuple.pair splitPoints (List.drop 1 splitPoints)
    in
    Cmd.none


view : Options -> TrackObservations.TrackObservations -> (Msg -> msg) -> Track -> Element msg
view options observations wrapper track =
    let
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
        [ text "Files will be written to Downloads folder."
        , partsSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| SplitTrack
            , label =
                text <|
                    "Split into "
                        ++ String.fromInt options.splitCount
                        ++ " files\n"
                        ++ "each "
                        ++ showDecimal2 options.splitLength
                        ++ "km long."
            }
        ]
