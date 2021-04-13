module Main exposing (main)

import Accordion exposing (AccordionEntry, AccordionState(..), accordionToggle, accordionView)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Element as E exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseTrackPoints)
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import MarkerControls exposing (markerButton)
import Nudge exposing (NudgeEffects(..), NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import SceneBuilder exposing (RenderingContext, Scene, defaultRenderingContext)
import ScenePainter exposing (ImageMsg, PostUpdateAction(..), ViewingContext, defaultViewingContext, initialiseView, viewWebGLContext)
import Task
import Time
import Track exposing (Track, trackPointNearestRay)
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
    | Undo
    | Redo


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
    , nudgePreview : Scene
    , completeScene : Scene
    , renderingContext : Maybe RenderingContext
    , viewingContext : Maybe ViewingContext
    , track : Maybe Track
    , toolsAccordion : List (AccordionEntry Msg)
    , nudgeSettings : NudgeSettings
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , changeCounter : Int
    }


init : Int -> ( Model, Cmd Msg )
init mflags =
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = Nothing
      , staticScene = []
      , visibleMarkers = []
      , nudgePreview = []
      , completeScene = []
      , renderingContext = Nothing
      , viewingContext = Nothing
      , toolsAccordion = []
      , nudgeSettings = defaultNudgeSettings
      , undoStack = []
      , redoStack = []
      , changeCounter = 0
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

        Undo ->
            ( model |> undo |> repaintTrack
            , Cmd.none
            )

        Redo ->
            ( model |> redo |> repaintTrack
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
                            Just <| initialiseView isTrack.track (trackPointNearestRay isTrack)

                        Nothing ->
                            Nothing
            in
            ( { model
                | track = track
                , staticScene = scene
                , visibleMarkers = markers
                , viewingContext = viewingContext
                , completeScene = markers ++ scene
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
                        , completeScene = updatedMarkers ++ model.nudgePreview ++ model.staticScene
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
                        ( newGraph, action ) =
                            Graph.update innerMsg isTrack.track isTrack.graph

                        newTrackPoints =
                            Maybe.map Graph.walkTheRoute newGraph
                                |> Maybe.withDefault []

                        newTrack =
                            { isTrack
                                | graph = newGraph
                                , track = newTrackPoints
                            }
                    in
                    ( case action of
                        GraphChanged undoMsg ->
                            model |> trackHasChanged undoMsg newTrack

                        GraphSettingsChanged ->
                            { model | track = Just newTrack }

                        _ ->
                            model
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

                        updatedMarkers =
                            SceneBuilder.renderMarkers newTrack
                    in
                    ( { model
                        | track = Just newTrack
                        , visibleMarkers = updatedMarkers
                        , completeScene = updatedMarkers ++ model.nudgePreview ++ model.staticScene
                        , nudgePreview = []
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NudgeMessage nudgeMsg ->
            case model.track of
                Just isTrack ->
                    let
                        ( newSetttings, newTrack, action ) =
                            Nudge.update nudgeMsg model.nudgeSettings isTrack
                    in
                    ( case action of
                        NudgeTrackChanged undoMsg ->
                            { model | nudgePreview = [] }
                                |> trackHasChanged undoMsg newTrack

                        NudgePreview points ->
                            let
                                newPreview =
                                    SceneBuilder.previewNudge points
                            in
                            { model
                                | nudgePreview = newPreview
                                , completeScene = newPreview ++ model.visibleMarkers ++ model.staticScene
                                , nudgeSettings = newSetttings
                            }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


trackHasChanged : String -> Track -> Model -> Model
trackHasChanged undoMsg newTrack oldModel =
    oldModel
        |> addToUndoStack undoMsg
        |> (\m -> { m | track = Just newTrack })
        |> repaintTrack


repaintTrack : Model -> Model
repaintTrack model =
    let
        updatedScene =
            Maybe.map2
                SceneBuilder.renderTrack
                model.renderingContext
                model.track
                |> Maybe.withDefault []

        updatedMarkers =
            Maybe.map SceneBuilder.renderMarkers model.track |> Maybe.withDefault []
    in
    { model
        | staticScene = updatedScene
        , visibleMarkers = updatedMarkers
        , completeScene = updatedMarkers ++ model.nudgePreview ++ updatedScene
        , viewingContext = Maybe.map2 refreshSceneSearcher model.viewingContext model.track
    }


refreshSceneSearcher : ViewingContext -> Track -> ViewingContext
refreshSceneSearcher context track =
    { context | sceneSearcher = trackPointNearestRay track }


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
                                model.completeScene
                                imageMessageWrapper
                            , column defaultColumnLayout
                                [ markerButton isTrack markerMessageWrapper
                                , undoRedoButtons model
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
    , { label = "The Labyrinth"
      , state = Contracted
      , content =
            case ( model.viewingContext, model.track ) of
                ( Just context, Just isTrack ) ->
                    viewGraphControls isTrack.graph graphMessageWrapper

                _ ->
                    none
      }
    ]



-- Reluctantly putting this here.


type alias UndoEntry =
    { label : String
    , track : Maybe Track
    }


addToUndoStack :
    String
    -> Model
    -> Model
addToUndoStack label model =
    { model
        | undoStack =
            { label = label
            , track = model.track
            }
                :: List.take 19 model.undoStack
        , redoStack = []
        , changeCounter = model.changeCounter + 1
    }


undo : Model -> Model
undo model =
    case model.undoStack of
        action :: undos ->
            { model
                | track = action.track
                , undoStack = undos
                , redoStack =
                    { action
                        | track = model.track
                    }
                        :: model.redoStack
                , changeCounter = model.changeCounter - 1
            }

        _ ->
            model


redo : Model -> Model
redo model =
    case model.redoStack of
        action :: redos ->
            { model
                | track = action.track
                , redoStack = redos
                , undoStack =
                    { action
                        | track = model.track
                    }
                        :: model.undoStack
                , changeCounter = model.changeCounter + 1
            }

        _ ->
            model


undoRedoButtons model =
    row
        defaultRowLayout
        [ button
            prettyButtonStyles
            { onPress =
                case model.undoStack of
                    [] ->
                        Nothing

                    _ ->
                        Just Undo
            , label =
                case model.undoStack of
                    u :: _ ->
                        E.text <| "Undo " ++ u.label

                    _ ->
                        E.text "Nothing to undo"
            }
        , button
            prettyButtonStyles
            { onPress =
                case model.redoStack of
                    [] ->
                        Nothing

                    _ ->
                        Just Redo
            , label =
                case model.redoStack of
                    u :: _ ->
                        E.text <| "Redo " ++ u.label

                    _ ->
                        E.text "Nothing to redo"
            }
        ]
