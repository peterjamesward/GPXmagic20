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
import SceneBuilder exposing (RenderingContext, Scene, defaultRenderingContext)
import ScenePainter exposing (ImageMsg, PostUpdateAction(..), ViewingContext, defaultViewingContext, initialiseView, viewWebGLContext)
import Task
import Time
import TrackPoint exposing (Track, TrackPoint, prepareTrackPoints, trackPointNearestRay)
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
    , graph : Graph
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
      , graph = Graph.emptyGraph
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
                    case track of
                        Just isTrack ->
                            SceneBuilder.render defaultRenderingContext isTrack

                        Nothing ->
                            []

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
                            -- Focus moved, so detailed area changes.
                            case model.renderingContext of
                                Just context ->
                                    SceneBuilder.render context updatedTrack

                                Nothing ->
                                    []
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
                    case model.viewingContext of
                        Just context ->
                            [ viewWebGLContext context model.staticScene imageMessageWrapper
                            , viewGraphControls model.graph graphMessageWrapper
                            ]

                        Nothing ->
                            [ none ]
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
