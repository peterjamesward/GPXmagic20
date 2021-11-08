module TrackSplitter exposing (..)

import BoundingBox3d
import Delay
import Element exposing (..)
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download
import File.Select as Select
import Length
import List.Extra
import OneClickQuickFix exposing (oneClickQuickFix)
import Point3d
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity
import Task
import Track exposing (Track)
import TrackEditType exposing (TrackEditType(..))
import TrackObservations
import TrackPoint
import Utils exposing (showDecimal0, showDecimal2, showLongMeasure, withLeadingZeros)
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, prettyButtonStyles, wideSliderStyles)
import WriteGPX


toolLabel =
    "Splitter & Joiner"


info =
    """## Splitter & Joiner

"Track splitter" can be used to deal with really long routes, such as you may encounter
if you tried to replicate a big Audax adventure. We know Magic Roads limits us to 100km, but you can set your
own limit on the length, and this will work out roughly equal splits within the limit you set. It writes them
out as new files in your Downloads folder but does not replace the loaded track.

"Allow for start and end pens" will put a 60m zone before the start and a 140m zone after the end
to allow for the RGT rider pens. This should mean you actually get to ride the relevant section in full.

"Append file" will allow you to select a GPX file on your computer to be appended to the current route.
This will **not** adjust the locations of either route; you can use Shift & Rotate for that.
"""


type Msg
    = SplitTrack
    | SetSplitLimit Length.Length
    | WriteSection (List ( Int, Float, Float ))
    | ToggleBuffers Bool
    | AppendFile
    | FileSelected File
    | FileLoaded String
    | ToggleAutofix Bool


type alias Options =
    { splitLimit : Length.Length
    , addBuffers : Bool
    , applyAutofix : Bool
    }


defaultOptions : Options
defaultOptions =
    { splitLimit = Length.kilometers 100.0
    , addBuffers = False
    , applyAutofix = False
    }


update :
    Msg
    -> Options
    -> TrackObservations.TrackObservations
    -> Maybe Track
    -> (Msg -> msg)
    -> ( Options, PostUpdateActions.PostUpdateAction (Cmd msg) )
update msg settings observations mTrack msgWrapper =
    case msg of
        SetSplitLimit n ->
            ( { settings
                | splitLimit = n
              }
            , ActionNoOp
            )

        ToggleBuffers state ->
            ( { settings
                | addBuffers = not settings.addBuffers
              }
            , ActionNoOp
            )

        ToggleAutofix _ ->
            ( { settings
                | applyAutofix = not settings.applyAutofix
              }
            , ActionNoOp
            )

        AppendFile ->
            ( settings
            , ActionCommand <| Cmd.map msgWrapper <| Select.file [ "text/gpx" ] FileSelected
            )

        FileSelected file ->
            ( settings
            , ActionCommand <| Task.perform (msgWrapper << FileLoaded) (File.toString file)
            )

        FileLoaded content ->
            -- You'd think we could just concatenate the track point lists.
            -- Tried that, but they have different bounding boxes and "Ghanians".
            -- Life might be easier just to spin up a new GPX string and start over!
            -- Let's see if we can fix this without that resort. We did, not elegantly.
            let
                track2 =
                    Track.trackFromGpx content

                newTrack =
                    case ( mTrack, track2 ) of
                        ( Just originalTrack, Just extension ) ->
                            let
                                -- No need to be efficient.
                                originalInGPS =
                                    Track.removeGhanianTransform originalTrack

                                extensionInGPS =
                                    Track.removeGhanianTransform extension

                                combinedInGPS =
                                    originalInGPS ++ extensionInGPS

                                combined =
                                    TrackPoint.applyGhanianTransform
                                        originalTrack.earthReferenceCoordinates
                                        combinedInGPS
                            in
                            Just
                                { originalTrack
                                    | trackPoints = combined |> TrackPoint.prepareTrackPoints
                                    , box =
                                        BoundingBox3d.hullOfN .xyz combined
                                            |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
                                }

                        _ ->
                            Nothing
            in
            case newTrack of
                Just isNewTrack ->
                    ( settings
                    , ActionTrackChanged EditPreservesIndex isNewTrack "Append file"
                    )

                Nothing ->
                    ( settings, ActionNoOp )

        SplitTrack ->
            case mTrack of
                Just track ->
                    ( settings
                    , ActionCommand <|
                        Delay.after 100 <|
                            msgWrapper <|
                                WriteSection <|
                                    writeSections
                                        track
                                        (Length.meters observations.trackLength)
                                        settings
                    )

                Nothing ->
                    ( settings
                    , ActionNoOp
                    )

        WriteSection sections ->
            case ( mTrack, sections ) of
                ( Just track, ( index, start, end ) :: rest ) ->
                    let
                        ( metricStart, metricEnd ) =
                            if settings.addBuffers then
                                ( Length.meters (start - 60.0)
                                , Length.meters (end + 140.0)
                                )

                            else
                                ( Length.meters start
                                , Length.meters end
                                )

                        trackName =
                            track.trackName |> Maybe.withDefault "track"

                        filename =
                            trackName
                                ++ "_"
                                ++ withLeadingZeros 2 (String.fromInt index)
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

                        processingFunction =
                            if settings.applyAutofix then
                                oneClickQuickFix

                            else
                                identity

                        content =
                            WriteGPX.writeGPX <| processingFunction trackExtract
                    in
                    ( settings
                    , ActionCommand <|
                        Cmd.batch
                            [ File.Download.string filename "text/xml" content
                            , Delay.after 2000 <| msgWrapper <| WriteSection rest
                            ]
                    )

                _ ->
                    ( settings, ActionNoOp )


writeSections : Track -> Length.Length -> Options -> List ( Int, Float, Float )
writeSections track length options =
    -- Doesn't *actually* split the track, just writes out the files.
    -- This function works out where the splits are, then each section is
    -- written out using the runtime, which kicks off the next.
    let
        effectiveLength =
            if options.addBuffers then
                options.splitLimit |> Quantity.minus (Length.meters 200.0)

            else
                options.splitLimit

        splitCount =
            ceiling <| Quantity.ratio length effectiveLength

        splitLength =
            length |> Quantity.divideBy (toFloat splitCount)

        splitPoints =
            List.map
                (\n ->
                    splitLength
                        |> Quantity.multiplyBy (toFloat n)
                        |> Length.inMeters
                )
                (List.range 0 splitCount)
    in
    List.map3
        (\a b c -> ( a, b, c ))
        (List.range 1 splitCount)
        splitPoints
        (List.drop 1 splitPoints)


view : Bool -> Options -> TrackObservations.TrackObservations -> (Msg -> msg) -> Track -> Element msg
view imperial options observations wrapper track =
    let
        effectiveLength =
            if options.addBuffers then
                options.splitLimit |> Quantity.minus (Length.meters 200.0)

            else
                options.splitLimit

        splitCount =
            ceiling <| Quantity.ratio (Length.meters observations.trackLength) effectiveLength

        splitLength =
            Length.meters observations.trackLength |> Quantity.divideBy (toFloat splitCount)

        partsSlider =
            if imperial then
                Input.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetSplitLimit << Length.miles
                    , label = Input.labelBelow [] <| text <| "Max: " ++ showLongMeasure True options.splitLimit
                    , min = 12.0
                    , max = 65.0
                    , step = Just 1.0
                    , value = Length.inMiles options.splitLimit
                    , thumb = Input.defaultThumb
                    }

            else
                Input.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetSplitLimit << Length.kilometers
                    , label = Input.labelBelow [] <| text <| "Max : " ++ showLongMeasure imperial options.splitLimit
                    , min = 20.0
                    , max = 100.0
                    , step = Just 1.0
                    , value = Length.inKilometers options.splitLimit
                    , thumb = Input.defaultThumb
                    }

        endPenCheckbox =
            Input.checkbox []
                { onChange = wrapper << ToggleBuffers
                , icon = checkboxIcon
                , checked = options.addBuffers
                , label = Input.labelRight [ centerY ] (text "Allow for start and end pens")
                }

        quickFixCheckbox =
            Input.checkbox []
                { onChange = wrapper << ToggleAutofix
                , icon = checkboxIcon
                , checked = options.applyAutofix
                , label = Input.labelRight [ centerY ] (text "Apply one-click-quick-fix to each section")
                }

        splitButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper <| SplitTrack
                , label =
                    text <|
                        "Split into "
                            ++ String.fromInt splitCount
                            ++ " files\n"
                            ++ "each "
                            ++ showLongMeasure imperial splitLength
                            ++ " long."
                }

        appendFileButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper <| AppendFile
                , label = text "Append file ..."
                }
    in
    wrappedRow [ spacing 10, padding 10 ]
        [ partsSlider
        , endPenCheckbox
        , quickFixCheckbox
        , splitButton
        , paragraph []
            [ text "Files will be written to Downloads folder at two second intervals." ]
        , appendFileButton
        ]
