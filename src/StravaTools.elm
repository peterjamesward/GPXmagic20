module StravaTools exposing (..)

import Element exposing (..)
import Element.Input as Input exposing (button)
import Http
import OAuthTypes as O exposing (Flow(..))
import PostUpdateActions
import StravaAuth exposing (getStravaToken)
import StravaDataLoad exposing (requestStravaRouteHeader)
import StravaTypes exposing (..)
import Track exposing (Track)
import Url exposing (Protocol(..), Url)
import ViewPureStyles exposing (prettyButtonStyles)


type Msg
    = UserChangedRouteId String
    | LoadExternalRoute
    | HandleSegmentData (Result Http.Error StravaSegment)
    | HandleSegmentStreams (Result Http.Error StravaSegmentStreams)
    | HandleRouteData (Result Http.Error StravaRoute)


type alias Options =
    { externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    }


defaultOptions : Options
defaultOptions =
    { externalSegmentId = ""
    , externalRouteId = ""
    , externalSegment = SegmentNone
    , stravaRoute = StravaRouteNone
    }


update :
    Msg
    -> Options
    -> O.Model
    -> (Msg -> msg)
    -> ( Options, PostUpdateActions.PostUpdateAction (Cmd msg) )
update msg settings authentication wrap =
    case msg of
        UserChangedRouteId url ->
            let
                routeId =
                    url |> String.split "/" |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            ( { settings | externalRouteId = routeId }
            , PostUpdateActions.ActionNoOp
            )

        LoadExternalRoute ->
            case getStravaToken authentication of
                Just token ->
                    ( { settings | stravaRoute = StravaRouteRequested }
                    , PostUpdateActions.ActionStravaFetch
                        (requestStravaRouteHeader
                            (wrap << HandleRouteData)
                            settings.externalRouteId
                            token
                        )
                    )

                Nothing ->
                    ( settings
                    , PostUpdateActions.ActionNoOp
                    )

        HandleRouteData response ->
            ( settings
            , PostUpdateActions.ActionNoOp
            )

        --case getStravaToken authentication of
        --    Just token ->
        --        let
        --            stravaRoute =
        --                stravaProcessRoute response
        --        in
        --        ( { model
        --            | stravaRoute = stravaRoute
        --            , filename = stravaRouteName stravaRoute
        --          }
        --        , requestStravaRoute GpxDownloaded model.externalRouteId token
        --        )
        --
        --    Nothing ->
        --        ( model, Cmd.none )
        HandleSegmentData response ->
            ( settings
            , PostUpdateActions.ActionNoOp
            )

        --( { model
        --    | externalSegment = stravaProcessSegment response model.trackPointBox
        --  }
        --, Cmd.none
        --)
        HandleSegmentStreams response ->
            ( settings
            , PostUpdateActions.ActionNoOp
            )



--case ( response, model.externalSegment ) of
--    ( Ok streams, SegmentOk segment ) ->
--        model
--            |> addToUndoStack "Paste Strava segment"
--            |> (\m -> { m | trackPoints = pasteStreams m.trackPoints segment streams })
--            |> trackHasChanged
--
--    ( Err err, _ ) ->
--        ( { model | lastHttpError = Just err }, Cmd.none )
--
--    _ ->
--        ( model, Cmd.none )


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
