module Main exposing (main)

import Accordion exposing (AccordionEntry, AccordionState(..), accordionToggle, accordionView)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseTrackPoints)
import Graph exposing (Graph, viewGraphControls)
import MarkerControls exposing (markerButton)
import Nudge exposing (NudgeSettings, defaultNudgeSettings, viewNudgeTools)
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
    | AccordionMessage (AccordionEntry Msg)
    | MarkerMessage MarkerControls.MarkerControlsMsg
    | NudgeMessage Nudge.NudgeMsg
    | Tick Time.Posix


imageMessageWrapper : ImageMsg -> Msg
imageMessageWrapper m =
    ImageMessage m


markerMessageWrapper : MarkerControls.MarkerControlsMsg -> Msg
markerMessageWrapper m =
    MarkerMessage m


graphMessageWrapper : Graph.Msg -> Msg
graphMessageWrapper m =
    GraphMessage m


nudgeMessageWrapper : Nudge.NudgeMsg -> Msg
nudgeMessageWrapper m =
    NudgeMessage m


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
    , visibleMarkers : Scene
    , renderingContext : Maybe RenderingContext
    , viewingContext : Maybe ViewingContext
    , track : Maybe Track
    , toolsAccordion : List (AccordionEntry Msg)
    , nudgeSettings : NudgeSettings
    }


init : Int -> ( Model, Cmd Msg )
init mflags =
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = Nothing
      , staticScene = []
      , visibleMarkers = []
      , renderingContext = Nothing
      , viewingContext = Nothing
      , toolsAccordion = []
      , nudgeSettings = defaultNudgeSettings
      }
    , Cmd.batch
        []
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

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
                        SceneBuilder.renderTrack
                        (Just defaultRenderingContext)
                        track
                        |> Maybe.withDefault []

                markers =
                    Maybe.map SceneBuilder.renderMarkers track |> Maybe.withDefault []

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
                , visibleMarkers = markers
                , viewingContext = viewingContext
                , renderingContext = Just defaultRenderingContext
                , toolsAccordion = toolsAccordion model
              }
            , Cmd.none
            )

        ImageMessage innerMsg ->
            let
                ( newContext, postUpdateAction ) =
                    case model.viewingContext of
                        Just context ->
                            ScenePainter.update innerMsg context model.time

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
                            -- Moving markers may affect detail level.
                            -- TODO: rebuild only if needed.
                            Maybe.map2
                                SceneBuilder.renderTrack
                                model.renderingContext
                                (Just updatedTrack)
                                |> Maybe.withDefault []

                        updatedMarkers =
                            SceneBuilder.renderMarkers updatedTrack
                    in
                    { model
                        | viewingContext = Just newContext
                        , track = Just updatedTrack
                        , staticScene = updatedScene
                        , visibleMarkers = updatedMarkers
                    }

                _ ->
                    model
            , Cmd.none
            )

        GraphMessage innerMsg ->
            case model.track of
                Just isTrack ->
                    let
                        ( newGraph, postUpdateAction ) =
                            Graph.update innerMsg isTrack.track isTrack.graph

                        newTrackPoints =
                            Maybe.map Graph.walkTheRoute newGraph
                                |> Maybe.withDefault []

                        newTrack =
                            { isTrack
                                | graph = newGraph
                                , track = newTrackPoints
                            }

                        updatedScene =
                            Maybe.map2
                                SceneBuilder.renderTrack
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

        AccordionMessage entry ->
            ( { model
                | toolsAccordion = accordionToggle model.toolsAccordion entry
              }
            , Cmd.none
            )

        MarkerMessage markerMsg ->
            case model.track of
                Just isTrack ->
                    let
                        newTrack =
                            MarkerControls.update markerMsg isTrack
                    in
                    ( { model
                        | track = Just newTrack
                        , visibleMarkers = SceneBuilder.renderMarkers newTrack
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NudgeMessage nudgeMsg ->
            case model.track of
                Just isTrack ->
                    let
                        ( newSetttings, newTrack ) =
                            Nudge.update nudgeMsg model.nudgeSettings isTrack

                        updatedScene =
                            if newTrack /= isTrack then
                                Maybe.map2
                                    SceneBuilder.renderTrack
                                    model.renderingContext
                                    (Just newTrack)
                                    |> Maybe.withDefault []

                            else
                                model.staticScene
                    in
                    ( { model
                        | track = Just newTrack
                        , nudgeSettings = newSetttings
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
                            [ viewWebGLContext
                                context
                                (model.staticScene ++ model.visibleMarkers)
                                imageMessageWrapper
                            , column defaultColumnLayout
                                [ markerButton isTrack markerMessageWrapper
                                , accordionView
                                    (updatedAccordion model model.toolsAccordion toolsAccordion)
                                    AccordionMessage
                                ]
                            ]

                        _ ->
                            [ none ]
                ]
        ]
    }


updatedAccordion model currentAccordion referenceAccordion =
    -- We have to reapply the accordion update functions with the current model,
    let
        blendAccordionStatus currentAccordionState refreshedContent =
            { currentAccordionState | content = refreshedContent.content }
    in
    List.map2
        blendAccordionStatus
        currentAccordion
        (referenceAccordion model)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 50 Tick
        ]


toolsAccordion model =
    [ --  { label = "Tip jar"
      --  , state = Contracted
      --  , content = tipJar
      --  }
      --, { label = "Loop maker"
      --  , state = Contracted
      --  , content = viewLoopTools model
      --  }
      --, { label = "Smooth bend"
      --  , state = Contracted
      --  , content = viewBendFixerPane model
      --  }
      --, { label = "Smooth gradient"
      --  , state = Contracted
      --  , content = viewGradientFixerPane model
      --  }
      { label = "Nudge "
      , state = Contracted
      , content = viewNudgeTools model.nudgeSettings nudgeMessageWrapper
      }

    --, { label = "Straighten"
    --  , state = Contracted
    --  , content = viewStraightenTools model
    --  }
    --, { label = "Trackpoints"
    --  , state = Contracted
    --  , content = viewTrackPointTools model
    --  }
    --, { label = "Fly-through"
    --  , state = Contracted
    --  , content = flythroughControls model
    --  }
    --, { label = "Strava"
    --  , state = Contracted
    --  , content = viewStravaDataAccessTab model
    --  }
    --, { label = "Filters"
    --  , state = Contracted
    --  , content = viewFilterControls model
    --  }
    , { label = "The Lab"
      , state = Contracted
      , content =
            case ( model.viewingContext, model.track ) of
                ( Just context, Just isTrack ) ->
                    viewGraphControls isTrack.graph graphMessageWrapper

                _ ->
                    none
      }
    ]
