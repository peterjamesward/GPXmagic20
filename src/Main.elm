module Main exposing (main)

import Accordion exposing (AccordionEntry, AccordionState(..), view)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Delay exposing (after)
import DeletePoints exposing (Action(..), viewDeleteTools)
import DisplayOptions exposing (DisplayOptions)
import Element as E exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseTrackPoints)
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import Json.Encode
import MapController exposing (..)
import MarkerControls exposing (markerButton, viewTrackControls)
import Nudge exposing (NudgeEffects(..), NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import PostUpdateActions exposing (PostUpdateAction(..))
import Scene exposing (Scene)
import SceneBuilder exposing (RenderingContext, defaultRenderingContext)
import SceneBuilderProfile
import StravaAuth exposing (getStravaToken)
import Task
import Time
import TipJar
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Url exposing (Url)
import ViewPane as ViewPane exposing (ViewPane, ViewPaneAction(..), ViewPaneMessage, defaultViewPane, diminishPane, enlargePane, refreshSceneSearcher, updatePointerInLinkedPanes)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, prettyButtonStyles, toolRowLayout)
import ViewingContext


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | GraphMessage Graph.Msg
    | AccordionMessage Accordion.Msg
    | MarkerMessage MarkerControls.Msg
    | NudgeMessage Nudge.NudgeMsg
    | Tick Time.Posix
    | Undo
    | Redo
    | DeleteMessage DeletePoints.Msg
    | ViewPaneMessage ViewPane.ViewPaneMessage
    | OAuthMessage OAuthMsg
    | MapMessage Json.Encode.Value
    | RepaintMap
    | DisplayOptionsMessage DisplayOptions.Msg


markerMessageWrapper : MarkerControls.Msg -> Msg
markerMessageWrapper m =
    MarkerMessage m


graphMessageWrapper : Graph.Msg -> Msg
graphMessageWrapper m =
    GraphMessage m


nudgeMessageWrapper : Nudge.NudgeMsg -> Msg
nudgeMessageWrapper m =
    NudgeMessage m


deleteMessageWrapper : DeletePoints.Msg -> Msg
deleteMessageWrapper m =
    DeleteMessage m


viewPaneMessageWrapper : ViewPane.ViewPaneMessage -> Msg
viewPaneMessageWrapper m =
    ViewPaneMessage m


displayOptionsMessageWrapper : DisplayOptions.Msg -> Msg
displayOptionsMessageWrapper m =
    DisplayOptionsMessage m


wrapAuthMessage : OAuthMsg -> Msg
wrapAuthMessage msg =
    OAuthMessage msg


main : Program (Maybe (List Int)) Model Msg
main =
    -- This is the 'main' from OAuth example/
    application
        { init = Maybe.map StravaAuth.convertBytes >> init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always (OAuthMessage NoOp)
        , onUrlChange = always (OAuthMessage NoOp)
        , view = view
        }


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , staticScene : Scene
    , visibleMarkers : Scene
    , completeScene : Scene
    , completeProfile : Scene
    , profileScene : Scene
    , profileMarkers : Scene
    , renderingContext : Maybe RenderingContext
    , viewPanes : List ViewPane
    , track : Maybe Track
    , toolsAccordion : List (AccordionEntry Msg)
    , nudgeSettings : NudgeSettings
    , nudgePreview : Scene
    , nudgeProfilePreview : Scene
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , changeCounter : Int
    , stravaAuthentication : O.Model
    , displayOptions : DisplayOptions.DisplayOptions
    }


init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    -- We stitch in the OAuth init stuff somehow here.
    let
        ( authData, authCmd ) =
            StravaAuth.init mflags origin navigationKey wrapAuthMessage
    in
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , track = Nothing
      , staticScene = []
      , profileScene = []
      , profileMarkers = []
      , visibleMarkers = []
      , nudgePreview = []
      , nudgeProfilePreview = []
      , completeScene = []
      , completeProfile = []
      , renderingContext = Nothing
      , viewPanes = ViewPane.defaultViewPanes
      , toolsAccordion = []
      , nudgeSettings = defaultNudgeSettings
      , undoStack = []
      , redoStack = []
      , changeCounter = 0
      , stravaAuthentication = authData
      , displayOptions = DisplayOptions.defaultDisplayOptions
      }
    , Cmd.batch
        [ authCmd
        , MapController.createMap MapController.defaultMapInfo
        ]
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
            processGpxLoaded content model

        ViewPaneMessage innerMsg ->
            case model.track of
                Just track ->
                    processViewPaneMessage innerMsg model track

                Nothing ->
                    ( model, Cmd.none )

        RepaintMap ->
            ( model, refreshMap )

        GraphMessage innerMsg ->
            ( Maybe.map (processGraphMessage innerMsg model) model.track
                |> Maybe.withDefault model
            , Cmd.none
            )

        AccordionMessage accordionMsg ->
            ( { model | toolsAccordion = Accordion.update accordionMsg model.toolsAccordion }
            , Cmd.none
            )

        MarkerMessage markerMsg ->
            let
                action =
                    Maybe.map (MarkerControls.update markerMsg) model.track
                        |> Maybe.withDefault ActionNoOp
            in
            processPostUpdateAction model action

        NudgeMessage nudgeMsg ->
            let
                ( newModel, action ) =
                    Maybe.map (processNudgeMessage nudgeMsg model) model.track
                        |> Maybe.withDefault ( model, ActionNoOp )
            in
            processPostUpdateAction newModel action

        DeleteMessage deleteMsg ->
            let
                action =
                    Maybe.map (DeletePoints.update deleteMsg) model.track
                        |> Maybe.withDefault ActionNoOp
            in
            processPostUpdateAction model action

        -- Delegate wrapped OAuthmessages. Be bowled over if this works first time. Or fiftieth.
        -- Maybe look after to see if there is yet a token. Easy way to know.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData
            in
            ( { model | stravaAuthentication = newAuthData }
            , Cmd.map OAuthMessage authCmd
            )

        MapMessage _ ->
            ( model, Cmd.none )

        DisplayOptionsMessage dispMsg ->
            let
                ( newOptions, action ) =
                    DisplayOptions.update model.displayOptions
                        dispMsg
                        displayOptionsMessageWrapper

                viewPanes =
                    case action of
                        DisplayOptions.ProfileChange value ->
                            ViewPane.mapOverProfileContexts
                                (ViewingContext.setExaggeration value)
                                model.viewPanes

                        DisplayOptions.NoOp ->
                            model.viewPanes
            in
            ( { model
                | displayOptions = newOptions
                , viewPanes = viewPanes
              }
                |> repaintTrack
            , Cmd.none
            )


processPostUpdateAction : Model -> PostUpdateAction -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    -- I doubt that will ever be true.
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType newTrack undoMsg ) ->
            ( model |> trackHasChanged undoMsg newTrack
            , Cmd.batch <| ViewPane.makeMapCommands newTrack model.viewPanes
            )

        ( Just track, ActionPointerMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }
            in
            ( { model | track = Just updatedTrack }
                |> repaintMarkers
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] []
                , Delay.after 50 RepaintMap
                ]
            )

        ( Just track, ActionFocusMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }
            in
            ( { model
                | track = Just updatedTrack
                , viewPanes = ViewPane.mapOverPanes (updatePointerInLinkedPanes tp) model.viewPanes
              }
                |> repaintMarkers
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] []
                , MapController.centreMapOnCurrent updatedTrack
                , Delay.after 50 RepaintMap
                ]
            )

        ( Just track, ActionMarkerMove maybeTp ) ->
            let
                updatedTrack =
                    { track | markedNode = maybeTp }
            in
            ( { model | track = Just updatedTrack }
                |> repaintMarkers
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] []
                , Delay.after 50 RepaintMap
                ]
            )

        ( Just track, ActionRepaintMap ) ->
            ( model
            , Delay.after 50 RepaintMap
            )

        _ ->
            ( model, Cmd.none )


processGpxLoaded : String -> Model -> ( Model, Cmd Msg )
processGpxLoaded content model =
    let
        track =
            Track.trackFromGpx content

        scene =
            Maybe.map
                (SceneBuilder.renderTrack model.displayOptions)
                track
                |> Maybe.withDefault []

        profile =
            Maybe.map
                (SceneBuilderProfile.renderTrack model.displayOptions)
                track
                |> Maybe.withDefault []

        markers =
            Maybe.map SceneBuilder.renderMarkers track
                |> Maybe.withDefault []

        profileMarkers =
            Maybe.map (SceneBuilderProfile.renderMarkers model.displayOptions) track
                |> Maybe.withDefault []

        ( newViewPanes, mapCommands ) =
            case track of
                Just isTrack ->
                    ( List.map (ViewPane.resetAllViews isTrack) model.viewPanes
                    , ViewPane.makeMapCommands isTrack model.viewPanes
                        ++ [ Delay.after 50 RepaintMap ]
                    )

                Nothing ->
                    ( model.viewPanes, [] )
    in
    ( { model
        | track = track
        , staticScene = scene
        , profileScene = profile
        , visibleMarkers = markers
        , viewPanes = newViewPanes
        , completeScene = markers ++ scene
        , completeProfile = profileMarkers ++ profile
        , renderingContext = Just defaultRenderingContext
        , toolsAccordion = toolsAccordion model
      }
    , Cmd.batch mapCommands
    )


processViewPaneMessage : ViewPaneMessage -> Model -> Track -> ( Model, Cmd Msg )
processViewPaneMessage innerMsg model track =
    let
        ( newPane, postUpdateAction ) =
            ViewPane.update innerMsg model.viewPanes model.time

        updatedViewPanes =
            ViewPane.updateViewPanes newPane model.viewPanes

        updatedModel =
            { model | viewPanes = updatedViewPanes }
    in
    case postUpdateAction of
        ViewPane.ImageAction innerAction ->
            processPostUpdateAction updatedModel innerAction

        ViewPane.ApplyToAllPanes f ->
            ( { updatedModel
                | viewPanes = ViewPane.mapOverPanes f updatedModel.viewPanes
              }
            , Delay.after 50 RepaintMap
            )

        ViewPane.PaneNoOp ->
            ( updatedModel, Cmd.none )


processGraphMessage : Graph.Msg -> Model -> Track -> Model
processGraphMessage innerMsg model isTrack =
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
    case action of
        GraphChanged undoMsg ->
            model |> trackHasChanged undoMsg newTrack

        GraphSettingsChanged ->
            { model | track = Just newTrack }

        _ ->
            model


processNudgeMessage : Nudge.NudgeMsg -> Model -> Track -> ( Model, PostUpdateAction )
processNudgeMessage nudgeMsg model isTrack =
    let
        ( newSetttings, action ) =
            Nudge.update nudgeMsg model.nudgeSettings isTrack

        newModel =
            case action of
                ActionTrackChanged editType newTrack undoMsg ->
                    { model
                        | nudgePreview = []
                        , nudgeProfilePreview = []
                    }
                        |> trackHasChanged undoMsg newTrack

                ActionPreview label points ->
                    let
                        newPreview =
                            SceneBuilder.previewNudge points

                        newProfilePreview =
                            SceneBuilderProfile.previewNudge model.displayOptions points
                    in
                    { model
                        | nudgePreview = newPreview
                        , nudgeProfilePreview = newProfilePreview
                        , completeScene = newPreview ++ model.visibleMarkers ++ model.staticScene
                        , completeProfile = newProfilePreview ++ model.profileMarkers ++ model.profileScene
                        , nudgeSettings = newSetttings
                    }

                _ ->
                    model
    in
    ( newModel, action )


trackHasChanged : String -> Track -> Model -> Model
trackHasChanged undoMsg newTrack oldModel =
    let
        pushUndoStack =
            oldModel |> addToUndoStack undoMsg

        withNewTrack =
            { pushUndoStack | track = Just newTrack }
    in
    repaintTrack withNewTrack


repaintMarkers : Model -> Model
repaintMarkers model =
    let
        updatedMarkers =
            Maybe.map SceneBuilder.renderMarkers model.track
                |> Maybe.withDefault []

        updatedProfileMarkers =
            Maybe.map (SceneBuilderProfile.renderMarkers model.displayOptions) model.track
                |> Maybe.withDefault []
    in
    case model.track of
        Just isTrack ->
            { model
                | visibleMarkers = updatedMarkers
                , profileMarkers = updatedProfileMarkers
                , completeScene = updatedMarkers ++ model.nudgePreview ++ model.staticScene
                , completeProfile = updatedProfileMarkers ++ model.nudgeProfilePreview ++ model.profileScene
                , viewPanes = ViewPane.mapOverAllContexts (refreshSceneSearcher isTrack) model.viewPanes
            }

        Nothing ->
            model


repaintTrack : Model -> Model
repaintTrack model =
    let
        updatedScene =
            Maybe.map
                (SceneBuilder.renderTrack model.displayOptions)
                model.track
                |> Maybe.withDefault []

        updatedProfile =
            Maybe.map
                (SceneBuilderProfile.renderTrack model.displayOptions)
                model.track
                |> Maybe.withDefault []

        updatedMarkers =
            Maybe.map SceneBuilder.renderMarkers model.track |> Maybe.withDefault []

        updatedProfileMarkers =
            Maybe.map (SceneBuilderProfile.renderMarkers model.displayOptions) model.track
                |> Maybe.withDefault []
    in
    case model.track of
        Just isTrack ->
            { model
                | staticScene = updatedScene
                , profileScene = updatedProfile
                , visibleMarkers = updatedMarkers
                , profileMarkers = updatedProfileMarkers
                , completeScene = updatedMarkers ++ model.nudgePreview ++ updatedScene
                , completeProfile = updatedProfileMarkers ++ model.profileScene
                , viewPanes = ViewPane.mapOverAllContexts (refreshSceneSearcher isTrack) model.viewPanes
            }

        Nothing ->
            model


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
                [ width fill ]
                [ row defaultRowLayout
                    [ button
                        prettyButtonStyles
                        { onPress = Just GpxRequested
                        , label = text "Load GPX from your computer"
                        }
                    ]
                , row (width fill :: defaultRowLayout) <|
                    [ el [ width fill, alignTop ] <|
                        viewAllPanes
                            model.viewPanes
                            model.displayOptions
                            ( model.completeScene, model.completeProfile )
                            viewPaneMessageWrapper
                    , el [ alignTop ] <|
                        column defaultColumnLayout
                            [ markerButton model.track markerMessageWrapper
                            , viewTrackControls markerMessageWrapper model.track
                            , undoRedoButtons model
                            , Accordion.view
                                (updatedAccordion model model.toolsAccordion toolsAccordion)
                                AccordionMessage
                            ]
                    ]
                ]
        ]
    }


viewAllPanes : List ViewPane -> DisplayOptions -> ( Scene, Scene ) -> (ViewPaneMessage -> Msg) -> Element Msg
viewAllPanes panes options ( scene, profile ) wrapper =
    wrappedRow [ width fill ] <|
        List.map
            (ViewPane.view ( scene, profile ) options wrapper)
            panes


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
        [ MapController.messageReceiver MapMessage
        , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        , Time.every 50 Tick
        ]


toolsAccordion model =
    [ --{ label = "Views "
      --  , state = Contracted
      --  , content = ViewPane.viewPaneTools viewPaneMessageWrapper
      --  , info = ViewPane.info
      --  , previewMaker = always []
      --  }
      --,
      { label = "Visual styles"
      , state = Contracted
      , content = DisplayOptions.viewDisplayOptions model.displayOptions displayOptionsMessageWrapper
      , info = DisplayOptions.info
      , previewMaker = always []
      }

    --TODO: Make "click to drag" normal mode for Map.
    --, { label = "Map options"
    --  , state = Contracted
    --  , content = viewMapOptions model
    --  }
    , { label = "Tip jar"
      , state = Contracted
      , content = TipJar.tipJar
      , info = TipJar.info
      , previewMaker = always []
      }

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
    , { label = "Nudge "
      , state = Contracted
      , content = viewNudgeTools model.nudgeSettings nudgeMessageWrapper
      , info = Nudge.info
      , previewMaker = always []
      }

    --, { label = "Straighten"
    --  , state = Contracted
    --  , content = viewStraightenTools model
    --  }
    , { label = "Delete"
      , state = Contracted
      , content = viewDeleteTools model.track deleteMessageWrapper
      , info = DeletePoints.info
      , previewMaker = always []
      }

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
            model.track
                |> Maybe.map .graph
                |> Maybe.andThen
                    (Just << viewGraphControls graphMessageWrapper)
                |> Maybe.withDefault none
      , info = Graph.info
      , previewMaker = always []
      }

    --, { label = "Summary"
    --  , state = Expanded
    --  , content = overviewSummary model
    --  }
    --, { label = "Road segment data"
    --  , state = Contracted
    --  , content = summaryData (lookupRoad model model.currentNode)
    --  }
    --, { label = "Gradient problems"
    --  , state = Contracted
    --  , content = viewGradientChanges model
    --  }
    --, { label = "Bend problems"
    --  , state = Contracted
    --  , content = viewBearingChanges model
    --  }
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
    row toolRowLayout
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
