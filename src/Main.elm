module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import Scene3d exposing (..)
import Task
import Time
import TrackPoint exposing (LocalCoords, TrackPoint)
import Url exposing (Url)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String


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
    , trackPoints : List TrackPoint
    , staticVisualEntities : List (Entity LocalCoords)
    , currentNode : Int
    }


init : Int -> ( Model, Cmd Msg )
init mflags =
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , trackPoints = []
      , trackName = Nothing
      , staticVisualEntities = []
      , currentNode = 0
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
            ( parseGPXintoModel model content
            , Cmd.none
            )


parseGPXintoModel : Model -> String -> Model
parseGPXintoModel model content =
    { model
        | trackName = Just "TEST"
        , trackPoints = []
    }


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
                []
                [ row [ spaceEvenly, spacing 10, padding 10 ]
                    [ button
                        []
                        { onPress = Just GpxRequested
                        , label = text "Load GPX from your computer"
                        }
                    ]
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
