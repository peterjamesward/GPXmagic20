module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseTrackPoints)
import SceneBuilder exposing (RenderingContext, Scene, defaultRenderingContext)
import ScenePainter exposing (ImageMsg, ViewingContext, defaultViewingContext, initialiseView, viewWebGLContext)
import Task
import Time
import TrackPoint exposing (Track, prepareTrackPoints)
import Url exposing (Url)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, prettyButtonStyles)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | ImageMessage ImageMsg


imageMessageWrapper : ImageMsg -> Msg
imageMessageWrapper m =
    ImageMessage m


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
    , trackName : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , track : Track
    , staticScene : Scene
    , currentNode : Int
    , renderingContext : RenderingContext
    , viewingContext : ViewingContext
    }


init : Int -> ( Model, Cmd Msg )
init mflags =
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = []
      , trackName = Nothing
      , staticScene = []
      , currentNode = 0
      , renderingContext = defaultRenderingContext
      , viewingContext = defaultViewingContext
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
                    content |> parseTrackPoints |> prepareTrackPoints

                scene =
                    SceneBuilder.render model.renderingContext track

                viewingContext =
                    initialiseView track
            in
            ( { model
                | trackName = Just "TEST"
                , track = track
                , staticScene = scene
                , viewingContext = viewingContext
              }
            , Cmd.none
            )

        ImageMessage innerMsg ->
            let
                ( newContext, currentPointChanged ) =
                    ScenePainter.update innerMsg model.viewingContext
            in
            ( { model
                | viewingContext = newContext
              }
            , Cmd.none
            )


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
                , row defaultRowLayout
                    [ viewWebGLContext model.viewingContext model.staticScene imageMessageWrapper
                    ]
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
