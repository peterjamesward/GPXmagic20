module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseTrackPoints)
import Graph exposing (Graph, viewGraphControls)
import Maybe.Extra as Maybe
import SceneBuilder exposing (RenderingContext, Scene, defaultRenderingContext)
import ScenePainter exposing (ImageMsg, PostUpdateAction(..), ViewingContext, defaultViewingContext, initialiseView, viewWebGLContext)
import Task
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, trackPointNearestRay)
import Url exposing (Url)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, prettyButtonStyles)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | ImageMessage ImageMsg
    | GraphMessage Graph.Msg


imageMessageWrapper : ImageMsg -> Msg
imageMessageWrapper m =
    ImageMessage m


graphMessageWrapper : Graph.Msg -> Msg
graphMessageWrapper m =
    GraphMessage m


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , staticScene : Scene
    , renderingContext : Maybe RenderingContext
    , viewingContext : Maybe ViewingContext
    , track : Maybe Track
    }


init : Int -> ( Model, Cmd Msg )
init mflags =
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = Nothing
      , staticScene = []
      , renderingContext = Nothing
      , viewingContext = Nothing
      }
    , Cmd.batch
        []
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GpxRequested ->
            ( model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            let
                track =
                    content |> parseTrackPoints

                scene =
                    Maybe.map2
                        SceneBuilder.render
                        (Just defaultRenderingContext)
                        track
                        |> Maybe.withDefault []

                viewingContext =
                    case track of
                        Just isTrack ->
                            Just <| initialiseView isTrack.track (trackPointNearestRay isTrack.track)

                        Nothing ->
                            Nothing
            in
            ( { model
                | track = track
                , staticScene = scene
                , viewingContext = viewingContext
                , renderingContext = Just defaultRenderingContext
              }
            , Cmd.none
            )

        ImageMessage innerMsg ->
            let
                ( newContext, postUpdateAction ) =
                    case model.viewingContext of
                        Just context ->
                            ScenePainter.update innerMsg context

                        Nothing ->
                            ( defaultViewingContext, NoContext )
            in
            ( case ( model.track, postUpdateAction ) of
                ( _, ImageOnly ) ->
                    { model | viewingContext = Just newContext }

                ( Just isTrack, PointerMove tp ) ->
                    let
                        updatedTrack =
                            { isTrack | currentNode = tp }

                        updatedScene =
                            Maybe.map2
                                SceneBuilder.render
                                model.renderingContext
                                (Just updatedTrack)
                                |> Maybe.withDefault []
                    in
                    { model
                        | viewingContext = Just newContext
                        , track = Just updatedTrack
                        , staticScene = updatedScene
                    }

                _ ->
                    model
            , Cmd.none
            )

        GraphMessage innerMsg ->
            case model.track of
                Just isTrack ->
                    let
                        ( newGraph, _ ) =
                            Graph.update innerMsg isTrack.track isTrack.graph

                        newTrack =
                            { isTrack | graph = newGraph }

                        updatedScene =
                            Maybe.map2
                                SceneBuilder.render
                                model.renderingContext
                                (Just newTrack)
                                |> Maybe.withDefault []
                    in
                    ( { model
                        | track = Just newTrack
                        , staticScene = updatedScene
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic 2.0"
    , body =
        [ layout
            [ width fill
            , padding 10
            , spacing 10
            , Font.size 16
            , height fill
            ]
          <|
            column
                defaultColumnLayout
                [ row defaultRowLayout
                    [ button
                        prettyButtonStyles
                        { onPress = Just GpxRequested
                        , label = text "Load GPX from your computer"
                        }
                    ]
                , row defaultRowLayout <|
                    case ( model.viewingContext, model.track ) of
                        ( Just context, Just isTrack ) ->
                            [ viewWebGLContext context model.staticScene imageMessageWrapper
                            , viewGraphControls isTrack.graph graphMessageWrapper
                            ]

                        _ ->
                            [ none ]
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
