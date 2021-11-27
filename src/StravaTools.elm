module StravaTools exposing (..)

import ColourPalette exposing (stravaOrange)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http
import List.Extra
import GpxSource exposing (GpxSource(..))
import OAuthTypes as O exposing (Flow(..))
import PostUpdateActions
import StravaAuth exposing (getStravaToken)
import StravaDataLoad exposing (..)
import StravaPasteStreams exposing (pasteStreams)
import StravaTypes exposing (..)
import Track exposing (Track, trackBoundingBox)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Url
import Url.Builder as Builder
import ViewPureStyles exposing (displayName, prettyButtonStyles)

toolLabel = "Strava"
info =
    """## Strava

You can replace a section of the route by a Strava segment.

This may or may not contain more accurate elevation data.

You will probably need to finesse the start and end of the segment.

The segment orientation should match the direction of travel.

It will not paste segments that are not close to the route (subject to change)."""


type Msg
    = UserChangedRouteId String
    | LoadExternalRoute
    | HandleSegmentData (Result Http.Error StravaSegment)
    | HandleSegmentStreams (Result Http.Error StravaSegmentStreams)
    | HandleRouteData (Result Http.Error StravaRoute)
    | GpxDownloaded (Result Http.Error String)
    | UserChangedSegmentId String
    | LoadSegmentStreams
    | LoadExternalSegment
    | PasteSegment
    | ClearSegment


type alias Options =
    { externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    , stravaStreams : Maybe StravaSegmentStreams
    , lastHttpError : Maybe Http.Error
    , preview : List TrackPoint
    }


defaultOptions : Options
defaultOptions =
    { externalSegmentId = ""
    , externalRouteId = ""
    , externalSegment = SegmentNone
    , stravaRoute = StravaRouteNone
    , stravaStreams = Nothing
    , lastHttpError = Nothing
    , preview = []
    }


update :
    Msg
    -> Options
    -> O.Model
    -> (Msg -> msg)
    -> Maybe Track
    -> ( Options, PostUpdateActions.PostUpdateAction (Cmd msg) )
update msg settings authentication wrap track =
    case msg of
        UserChangedRouteId url ->
            let
                routeId =
                    url
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault ""
            in
            ( { settings | externalRouteId = routeId }
            , PostUpdateActions.ActionNoOp
            )

        UserChangedSegmentId url ->
            let
                segmentId =
                    url
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault ""
            in
            ( { settings
                | externalSegmentId = segmentId
                , externalSegment = SegmentNone
              }
            , PostUpdateActions.ActionNoOp
            )

        LoadExternalRoute ->
            case getStravaToken authentication of
                Just token ->
                    ( { settings | stravaRoute = StravaRouteRequested }
                    , PostUpdateActions.ActionCommand <|
                        requestStravaRouteHeader
                            (wrap << HandleRouteData)
                            settings.externalRouteId
                            token
                    )

                Nothing ->
                    ( settings
                    , PostUpdateActions.ActionNoOp
                    )

        HandleRouteData response ->
            case getStravaToken authentication of
                Just token ->
                    let
                        stravaRoute =
                            stravaProcessRoute response
                    in
                    ( { settings | stravaRoute = stravaRoute }
                    , PostUpdateActions.ActionCommand <|
                        requestStravaRoute
                            (wrap << GpxDownloaded)
                            settings.externalRouteId
                            token
                    )

                Nothing ->
                    ( settings, PostUpdateActions.ActionNoOp )

        GpxDownloaded response ->
            case response of
                Ok content ->
                    ( settings, PostUpdateActions.ActionNewRoute content GpxStrava )

                Err _ ->
                    ( settings, PostUpdateActions.ActionNoOp )

        LoadExternalSegment ->
            case getStravaToken authentication of
                Just token ->
                    ( { settings | externalSegment = SegmentRequested }
                    , PostUpdateActions.ActionCommand <|
                        requestStravaSegment
                            (wrap << HandleSegmentData)
                            settings.externalSegmentId
                            token
                    )

                Nothing ->
                    ( settings, PostUpdateActions.ActionNoOp )

        LoadSegmentStreams ->
            case getStravaToken authentication of
                Just token ->
                    ( settings
                    , PostUpdateActions.ActionCommand <|
                        requestStravaSegmentStreams
                            (wrap << HandleSegmentStreams)
                            settings.externalSegmentId
                            token
                    )

                Nothing ->
                    ( settings, PostUpdateActions.ActionNoOp )

        HandleSegmentData response ->
            case track of
                Just isTrack ->
                    ( { settings
                        | externalSegment =
                            stravaProcessSegment
                                response
                                (trackBoundingBox isTrack)
                      }
                    , PostUpdateActions.ActionNoOp
                    )

                Nothing ->
                    ( settings, PostUpdateActions.ActionNoOp )

        HandleSegmentStreams response ->
            case ( track, response, settings.externalSegment ) of
                ( Just isTrack, Ok streams, SegmentOk segment ) ->
                    ( { settings
                        | stravaStreams = Just streams
                        , externalSegment = SegmentPreviewed segment
                      }
                    , PostUpdateActions.ActionPreview
                    )

                ( _, Err err, _ ) ->
                    ( { settings | lastHttpError = Just err }
                    , PostUpdateActions.ActionNoOp
                    )

                _ ->
                    ( settings, PostUpdateActions.ActionNoOp )

        PasteSegment ->
            case ( track, settings.externalSegment, settings.stravaStreams ) of
                ( Just isTrack, SegmentPreviewed segment, Just streams ) ->
                    ( { settings
                        | stravaStreams = Nothing
                        , externalSegment = SegmentNone
                      }
                    , PostUpdateActions.ActionTrackChanged
                        PostUpdateActions.EditPreservesIndex
                        { isTrack | trackPoints = pasteStreams isTrack segment streams }
                        "Paste Strava segment"
                    )

                _ ->
                    ( settings, PostUpdateActions.ActionNoOp )

        ClearSegment ->
            ( { settings | stravaStreams = Nothing, externalSegment = SegmentNone }
            , PostUpdateActions.ActionPreview
            )


stravaRouteOption : O.Model -> Options -> (Msg -> msg) -> Element msg
stravaRouteOption auth options wrap =
    let
        routeIdField =
            Input.text [ width (px 150) ]
                { onChange = wrap << UserChangedRouteId
                , text = options.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| text "Strava route ID"
                , label = Input.labelHidden "Strava route ID"
                }

        routeButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrap LoadExternalRoute
                , label = text <| "Fetch route"
                }
    in
    case getStravaToken auth of
        Just token ->
            row [ spacing 10 ]
                [ routeIdField
                , routeButton
                ]

        Nothing ->
            none


viewStravaTab : Options -> (Msg -> msg) -> Track -> Element msg
viewStravaTab options wrap track =
    let
        segmentIdField =
            Input.text []
                { onChange = wrap << UserChangedSegmentId
                , text = options.externalSegmentId
                , placeholder = Just <| Input.placeholder [] <| text "Segment ID"
                , label = Input.labelHidden "Segment ID"
                }

        segmentButton =
            -- Make this button serve two functions.
            -- 1. After a URL change, to load the segment header;
            -- 2. After header loaded, to load and paste the streams.
            case options.externalSegment of
                SegmentOk segment ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap LoadSegmentStreams
                        , label = text "Preview"
                        }

                SegmentPreviewed segment ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap PasteSegment
                        , label = text "Paste"
                        }

                SegmentNone ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap LoadExternalSegment
                        , label = text <| "Fetch header"
                        }

                SegmentNotInRoute _ ->
                    text "This segment is not\ncontained in the route"

                _ ->
                    none

        clearButton =
            case options.externalSegment of
                SegmentNone ->
                    none

                _ ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap ClearSegment
                        , label = text "Clear"
                        }

        segmentInfo =
            case options.externalSegment of
                SegmentRequested ->
                    text "Waiting for segment"

                SegmentError err ->
                    text err

                SegmentNone ->
                    text "Segment data not loaded, or not yet."

                SegmentOk segment ->
                    text segment.name

                SegmentPreviewed segment ->
                    text "In preview"

                SegmentNotInRoute segment ->
                    text segment.name

        stravaLink =
            let
                stravaUrl =
                    Builder.crossOrigin stravaApiRoot [ "routes", options.externalRouteId ] []
            in
            case options.stravaRoute of
                StravaRouteOk _ ->
                    column [ Font.size 14, padding 5 ]
                        [ displayName <| Just options.externalRouteId
                        , newTabLink [ Font.color stravaOrange ]
                            { url = stravaUrl
                            , label = text "View on Strava"
                            }
                        ]

                _ ->
                    none
    in
    column [ spacing 10, padding 10, width fill ]
        [ stravaLink
        , row [ spacing 10 ]
            [ segmentIdField
            , segmentButton
            , clearButton
            ]
        , segmentInfo
        ]


preview : Options -> Track -> List TrackPoint
preview options track =
    case options.stravaStreams of
        Just streams ->
            StravaPasteStreams.pointsFromStreams streams track

        Nothing ->
            []
