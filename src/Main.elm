module Main exposing (main)

import Accordion exposing (AccordionEntry, AccordionState(..), view)
import BendSmoother exposing (SmoothedBend, lookForSmoothBendOption)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Delay exposing (after)
import DeletePoints exposing (Action(..), viewDeleteTools)
import DisplayOptions exposing (DisplayOptions)
import Element as E exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Filters
import Flythrough exposing (Flythrough)
import GradientSmoother
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import InsertPoints
import Json.Encode
import Loop
import MapController exposing (..)
import MarkerControls exposing (markerButton, viewTrackControls)
import Maybe.Extra
import Nudge exposing (NudgeEffects(..), NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import OAuth.GpxSource exposing (GpxSource(..))
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import PostUpdateActions exposing (PostUpdateAction(..))
import Scene exposing (Scene)
import SceneBuilder exposing (RenderingContext, defaultRenderingContext)
import SceneBuilderProfile
import Straightener
import StravaAuth exposing (getStravaToken, stravaButton)
import StravaTools exposing (stravaRouteOption)
import Task
import Time
import TipJar
import Track exposing (Track, summaryData)
import TrackObservations exposing (TrackObservations, deriveProblems)
import TrackPoint exposing (TrackPoint, prepareTrackPoints)
import Url exposing (Url)
import ViewPane as ViewPane exposing (ViewPane, ViewPaneAction(..), ViewPaneMessage, defaultViewPane, diminishPane, enlargePane, refreshSceneSearcher, updatePointerInLinkedPanes)
import ViewPureStyles exposing (defaultColumnLayout, defaultRowLayout, displayName, prettyButtonStyles, toolRowLayout)
import ViewingContext exposing (ViewingContext)
import WriteGPX exposing (writeGPX)


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
    | BendSmoothMessage BendSmoother.Msg
    | LoopMsg Loop.Msg
    | GradientMessage GradientSmoother.Msg
    | StraightenMessage Straightener.Msg
    | FlythroughMessage Flythrough.Msg
    | FilterMessage Filters.Msg
    | ProblemMessage TrackObservations.Msg
    | InsertMessage InsertPoints.Msg
    | UserChangedFilename String
    | OutputGPX
    | StravaMessage StravaTools.Msg


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


bendSmootherMessageWrapper : BendSmoother.Msg -> Msg
bendSmootherMessageWrapper m =
    BendSmoothMessage m


wrapAuthMessage : OAuthMsg -> Msg
wrapAuthMessage msg =
    OAuthMessage msg


loopMessageWrapper : Loop.Msg -> Msg
loopMessageWrapper msg =
    LoopMsg msg


gradientMessageWrapper : GradientSmoother.Msg -> Msg
gradientMessageWrapper msg =
    GradientMessage msg


straightenMessageWrapper : Straightener.Msg -> Msg
straightenMessageWrapper msg =
    StraightenMessage msg


flythroughMessageWrapper : Flythrough.Msg -> Msg
flythroughMessageWrapper msg =
    FlythroughMessage msg


filterMessageWrapper : Filters.Msg -> Msg
filterMessageWrapper msg =
    FilterMessage msg


problemMessageWrapper : TrackObservations.Msg -> Msg
problemMessageWrapper msg =
    ProblemMessage msg


insertMessageWrapper : InsertPoints.Msg -> Msg
insertMessageWrapper msg =
    InsertMessage msg


stravaMessageWrapper : StravaTools.Msg -> Msg
stravaMessageWrapper msg =
    StravaMessage msg


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
    , gpxSource : GpxSource
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
    , displayOptions : DisplayOptions.DisplayOptions
    , bendOptions : BendSmoother.BendOptions
    , bendPreview : Scene
    , observations : TrackObservations
    , gradientOptions : GradientSmoother.Options
    , straightenOptions : Straightener.Options
    , flythrough : Flythrough.Options
    , filterOptions : Filters.Options
    , problemOptions : TrackObservations.Options
    , insertOptions : InsertPoints.Options
    , stravaOptions : StravaTools.Options
    , stravaAuthentication : O.Model
    }


init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    -- We stitch in the OAuth init stuff somehow here.
    let
        ( authData, authCmd ) =
            StravaAuth.init mflags origin navigationKey wrapAuthMessage
    in
    ( { filename = Nothing
      , gpxSource = GpxNone
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
      , displayOptions = DisplayOptions.defaultDisplayOptions
      , bendOptions = BendSmoother.defaultOptions
      , bendPreview = []
      , observations = TrackObservations.defaultObservations
      , gradientOptions = GradientSmoother.defaultOptions
      , straightenOptions = Straightener.defaultOptions
      , flythrough = Flythrough.defaultOptions
      , filterOptions = Filters.defaultOptions
      , problemOptions = TrackObservations.defaultOptions
      , insertOptions = InsertPoints.defaultOptions
      , stravaOptions = StravaTools.defaultOptions
      , stravaAuthentication = authData
      }
    , Cmd.batch
        [ authCmd
        , MapController.createMap MapController.defaultMapInfo
        ]
    )


passFlythroughToContext : Maybe Flythrough -> ViewingContext -> ViewingContext
passFlythroughToContext flight context =
    { context | flythrough = flight }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                flythrough =
                    model.flythrough

                updatedFlythrough =
                    Flythrough.advanceFlythrough
                        newTime
                        { flythrough | modelTime = newTime }
            in
            ( { model
                | time = newTime
                , flythrough = updatedFlythrough

                -- This passing through time is perhaps not the best idea.
                , viewPanes =
                    ViewPane.mapOverAllContexts
                        (passFlythroughToContext updatedFlythrough.flythrough)
                        model.viewPanes
              }
            , Cmd.none
            )

        Undo ->
            processPostUpdateAction (undo model) ActionRerender

        Redo ->
            processPostUpdateAction (redo model) ActionRerender

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
            Maybe.map (processViewPaneMessage innerMsg model) model.track
                |> Maybe.withDefault ( model, Cmd.none )

        RepaintMap ->
            ( model, refreshMap )

        GraphMessage innerMsg ->
            ( Maybe.map (processGraphMessage innerMsg model) model.track
                |> Maybe.withDefault model
            , Cmd.none
            )

        AccordionMessage accordionMsg ->
            processPostUpdateAction
                { model | toolsAccordion = Accordion.update accordionMsg model.toolsAccordion }
                PostUpdateActions.ActionPreview

        MarkerMessage markerMsg ->
            let
                action =
                    Maybe.map (MarkerControls.update markerMsg) model.track
                        |> Maybe.withDefault ActionNoOp
            in
            processPostUpdateAction model action

        NudgeMessage nudgeMsg ->
            let
                ( newSetttings, action ) =
                    Maybe.map (Nudge.update nudgeMsg model.nudgeSettings) model.track
                        |> Maybe.withDefault ( model.nudgeSettings, ActionNoOp )

                updateSettings m =
                    { m | nudgeSettings = newSetttings }
            in
            processPostUpdateAction (updateSettings model) action

        InsertMessage insertMsg ->
            let
                ( newSettings, action ) =
                    Maybe.map
                        (InsertPoints.update
                            insertMsg
                            model.insertOptions
                        )
                        model.track
                        |> Maybe.withDefault ( model.insertOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | insertOptions = newSettings }
                action

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
                |> renderTrackSceneElements
            , Cmd.none
            )

        BendSmoothMessage bendMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map (BendSmoother.update bendMsg model.bendOptions) model.track
                        |> Maybe.withDefault ( model.bendOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | bendOptions = newOptions }
                action

        LoopMsg loopMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map (Loop.update loopMsg model.observations.loopiness) model.track
                        |> Maybe.withDefault ( model.observations.loopiness, ActionNoOp )

                oldObs =
                    model.observations

                newObs =
                    { oldObs | loopiness = newOptions }
            in
            processPostUpdateAction
                { model | observations = newObs }
                action

        GradientMessage gradMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map (GradientSmoother.update gradMsg model.gradientOptions)
                        model.track
                        |> Maybe.withDefault ( model.gradientOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | gradientOptions = newOptions }
                action

        StraightenMessage straight ->
            let
                ( newOptions, action ) =
                    Maybe.map (Straightener.update straight model.straightenOptions)
                        model.track
                        |> Maybe.withDefault ( model.straightenOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | straightenOptions = newOptions }
                action

        FlythroughMessage flythroughMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (Flythrough.update
                            model.flythrough
                            flythroughMsg
                            flythroughMessageWrapper
                        )
                        model.track
                        |> Maybe.withDefault ( model.flythrough, ActionNoOp )
            in
            processPostUpdateAction
                { model
                    | flythrough = newOptions
                    , viewPanes =
                        ViewPane.mapOverAllContexts
                            (passFlythroughToContext newOptions.flythrough)
                            model.viewPanes
                }
                action

        FilterMessage filter ->
            let
                ( newOptions, action ) =
                    Maybe.map (Filters.update filter model.filterOptions model.observations)
                        model.track
                        |> Maybe.withDefault ( model.filterOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | filterOptions = newOptions }
                action

        ProblemMessage probMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (TrackObservations.update
                            probMsg
                            model.problemOptions
                            model.observations
                        )
                        model.track
                        |> Maybe.withDefault ( model.problemOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | problemOptions = newOptions }
                action

        UserChangedFilename txt ->
            ( { model | filename = Just txt }
            , Cmd.none
            )

        OutputGPX ->
            ( { model | changeCounter = 0 }
            , outputGPX model
            )

        StravaMessage stravaMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (StravaTools.update
                            stravaMsg
                            model.stravaOptions
                            model.stravaAuthentication
                            stravaMessageWrapper
                        )
                        model.track
                        |> Maybe.withDefault ( model.stravaOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | stravaOptions = newOptions }
                action


processPostUpdateAction : Model -> PostUpdateAction (Cmd Msg) -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    -- I doubt that will ever be true.
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType newTrack undoMsg ) ->
            ( model
                |> addToUndoStack undoMsg
                |> updateTrackInModel newTrack
            , Cmd.batch <| ViewPane.makeMapCommands newTrack model.viewPanes
            )

        ( Just track, ActionRerender ) ->
            ( model
                |> renderTrackSceneElements
            , Cmd.batch <| ViewPane.makeMapCommands track model.viewPanes
            )

        ( Just track, ActionPointerMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }
            in
            ( { model | track = Just updatedTrack }
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] [] ]
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
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] []
                , if ViewPane.mapPaneIsLinked model.viewPanes then
                    MapController.centreMapOnCurrent updatedTrack

                  else
                    Cmd.none
                ]
            )

        ( Just track, ActionMarkerMove maybeTp ) ->
            let
                updatedTrack =
                    { track | markedNode = maybeTp }
            in
            ( { model | track = Just updatedTrack }
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack [] [] ]
            )

        ( Just track, ActionRepaintMap ) ->
            ( model
            , Delay.after 50 RepaintMap
            )

        ( Just track, ActionPreview ) ->
            ( model |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap track [] [] ]
            )

        ( _, ActionStravaFetch a ) ->
            ( model, a )

        ( _, ActionNewRoute content source ) ->
            { model | gpxSource = source }
                |> processGpxLoaded content

        _ ->
            ( model, Cmd.none )


processGpxLoaded : String -> Model -> ( Model, Cmd Msg )
processGpxLoaded content model =
    let
        track =
            Track.trackFromGpx content

        ( newViewPanes, mapCommands ) =
            case track of
                Just isTrack ->
                    ( List.map (ViewPane.resetAllViews isTrack) model.viewPanes
                    , ViewPane.initialiseMap isTrack model.viewPanes
                        ++ [ Delay.after 50 RepaintMap ]
                    )

                Nothing ->
                    ( model.viewPanes, [] )
    in
    ( { model
        | track = track
        , renderingContext = Just defaultRenderingContext
        , toolsAccordion = toolsAccordion model
        , viewPanes = newViewPanes
        , gpxSource = GpxLocalFile
      }
        |> repeatTrackDerivations
    , Cmd.batch mapCommands
    )


processViewPaneMessage : ViewPaneMessage -> Model -> Track -> ( Model, Cmd Msg )
processViewPaneMessage innerMsg model track =
    let
        ( newPane, postUpdateAction ) =
            ViewPane.update innerMsg model.viewPanes viewPaneMessageWrapper

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

        newModel : Model -> Model
        newModel m =
            { m | track = Just newTrack }
    in
    case action of
        GraphChanged undoMsg ->
            model |> addToUndoStack undoMsg |> newModel

        GraphSettingsChanged ->
            model |> newModel

        _ ->
            model


updateTrackInModel : Track -> Model -> Model
updateTrackInModel track model =
    { model | track = Just track }
        |> repeatTrackDerivations


repeatTrackDerivations : Model -> Model
repeatTrackDerivations model =
    case model.track of
        Just isTrack ->
            let
                newTrack =
                    { isTrack | track = prepareTrackPoints isTrack.track }

                newObservations =
                    deriveProblems newTrack model.problemOptions
            in
            { model
                | track = Just newTrack
                , observations = newObservations
            }
                |> renderTrackSceneElements

        Nothing ->
            model


composeScene : Model -> Model
composeScene model =
    { model
        | completeScene =
            model.visibleMarkers
                ++ model.nudgePreview
                ++ model.bendPreview
                ++ model.staticScene
        , completeProfile =
            model.profileMarkers
                ++ model.nudgeProfilePreview
                ++ model.profileScene
    }


renderVaryingSceneElements : Model -> Model
renderVaryingSceneElements model =
    let
        updatedMarkers =
            Maybe.map SceneBuilder.renderMarkers model.track
                |> Maybe.withDefault []

        updatedProfileMarkers =
            Maybe.map (SceneBuilderProfile.renderMarkers model.displayOptions) model.track
                |> Maybe.withDefault []

        updatedNudgePreview =
            if
                Accordion.tabIsOpen "Nudge" model.toolsAccordion
                    && Nudge.settingNotZero model.nudgeSettings
            then
                Maybe.map (Nudge.previewNudgeNodes model.nudgeSettings) model.track
                    |> Maybe.withDefault []

            else
                []

        updatedBendOptions =
            let
                options =
                    model.bendOptions
            in
            if Accordion.tabIsOpen "Bend smoother classic" model.toolsAccordion then
                Maybe.map (tryBendSmoother options) model.track
                    |> Maybe.withDefault options

            else
                { options | smoothedBend = Nothing }

        updatedStraightenOptions =
            let
                options =
                    model.straightenOptions
            in
            if Accordion.tabIsOpen "Straighten" model.toolsAccordion then
                Maybe.map (Straightener.lookForSimplifications options) model.track
                    |> Maybe.withDefault options

            else
                options
    in
    { model
        | visibleMarkers = updatedMarkers
        , profileMarkers = updatedProfileMarkers
        , nudgePreview = SceneBuilder.previewNudge updatedNudgePreview
        , bendOptions = updatedBendOptions
        , bendPreview =
            Maybe.map
                (.nodes >> SceneBuilder.previewBend)
                updatedBendOptions.smoothedBend
                |> Maybe.withDefault []
        , nudgeProfilePreview =
            SceneBuilderProfile.previewNudge
                model.displayOptions
                updatedNudgePreview
        , straightenOptions = updatedStraightenOptions
    }
        |> composeScene


renderTrackSceneElements : Model -> Model
renderTrackSceneElements model =
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
    in
    case model.track of
        Just isTrack ->
            { model
                | staticScene = updatedScene
                , profileScene = updatedProfile
                , viewPanes = ViewPane.mapOverAllContexts (refreshSceneSearcher isTrack) model.viewPanes
            }
                |> renderVaryingSceneElements

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
                [ row [ spacing 10, padding 10 ]
                    [ button
                        prettyButtonStyles
                        { onPress = Just GpxRequested
                        , label = text "Load GPX from your computer"
                        }
                    , if model.changeCounter == 0 then
                        stravaButton
                            model.stravaAuthentication
                            wrapAuthMessage

                      else
                        E.text "Save your work before\nconnecting to Strava"
                    , stravaRouteOption
                        model.stravaAuthentication
                        model.stravaOptions
                        stravaMessageWrapper
                    , viewAndEditFilename model
                    , saveButtonIfChanged model
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
    if model.flythrough.flythrough /= Nothing then
        Sub.batch
            [ MapController.messageReceiver MapMessage
            , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
            , Time.every 50 Tick
            ]
    else
        Sub.batch
            [ MapController.messageReceiver MapMessage
            , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
            ]


toolsAccordion : Model -> List (AccordionEntry Msg)
toolsAccordion model =
    [ -- For V2 we see if a single collection works...
      { label = "Tip jar"
      , state = Contracted
      , content = TipJar.tipJar
      , info = TipJar.info
      }
    , { label = "Visual styles"
      , state = Contracted
      , content = DisplayOptions.viewDisplayOptions model.displayOptions displayOptionsMessageWrapper
      , info = DisplayOptions.info
      }
    , { label = "Loop maker"
      , state = Contracted
      , content = Loop.viewLoopTools model.observations.loopiness model.track loopMessageWrapper
      , info = Loop.info
      }
    , { label = "Bend smoother classic"
      , state = Contracted
      , content = BendSmoother.viewBendFixerPane model.bendOptions bendSmootherMessageWrapper
      , info = BendSmoother.info
      }
    , { label = "Smooth gradient"
      , state = Contracted
      , content =
            Maybe.map
                (GradientSmoother.viewGradientFixerPane
                    model.gradientOptions
                    gradientMessageWrapper
                )
                model.track
                |> Maybe.withDefault none
      , info = GradientSmoother.info
      }
    , { label = "Nudge"
      , state = Contracted
      , content = viewNudgeTools model.nudgeSettings nudgeMessageWrapper
      , info = Nudge.info
      }
    , { label = "Straighten"
      , state = Contracted
      , content =
            Maybe.map
                (Straightener.viewStraightenTools
                    model.straightenOptions
                    straightenMessageWrapper
                )
                model.track
                |> Maybe.withDefault none
      , info = Straightener.info
      }
    , { label = "Insert"
      , state = Contracted
      , content =
            case model.track of
                Just _ ->
                    InsertPoints.viewTools
                        model.insertOptions
                        insertMessageWrapper

                Nothing ->
                    none
      , info = InsertPoints.info
      }
    , { label = "Delete"
      , state = Contracted
      , content = viewDeleteTools model.track deleteMessageWrapper
      , info = DeletePoints.info
      }
    , { label = "Fly-through"
      , state = Contracted
      , content = Flythrough.flythroughControls model.flythrough flythroughMessageWrapper
      , info = Flythrough.info
      }
    , { label = "Filters"
      , state = Contracted
      , content =
            Maybe.map
                (Filters.viewFilterControls model.filterOptions
                    filterMessageWrapper
                )
                model.track
                |> Maybe.withDefault none
      , info = Filters.info
      }
    , { label = "The Labyrinth"
      , state = Contracted
      , content =
            model.track
                |> Maybe.map .graph
                |> Maybe.andThen
                    (Just << viewGraphControls graphMessageWrapper)
                |> Maybe.withDefault none
      , info = Graph.info
      }
    , { label = "Route summary"
      , state = Expanded False
      , content = TrackObservations.overviewSummary model.observations
      , info = "Data about the route."
      }
    , { label = "Road segment"
      , state = Contracted
      , content =
            Maybe.map summaryData model.track
                |> Maybe.withDefault none
      , info = "Data about the road at the orange marker."
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content =
            TrackObservations.viewGradientChanges
                model.problemOptions
                model.observations
                problemMessageWrapper
      , info = TrackObservations.info
      }
    , { label = "Bend problems"
      , state = Contracted
      , content =
            TrackObservations.viewBearingChanges
                model.problemOptions
                model.observations
                problemMessageWrapper
      , info = TrackObservations.info
      }
    , { label = "Strava"
      , state = Contracted
      , content =
            Maybe.map
                (StravaTools.viewStravaTab model.stravaOptions stravaMessageWrapper)
                model.track
                |> Maybe.withDefault none
      , info = StravaTools.info
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


tryBendSmoother : BendSmoother.BendOptions -> Track -> BendSmoother.BendOptions
tryBendSmoother options track =
    -- This, sadly, still here because import loops.
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.currentNode.index <= marker.index then
                ( track.currentNode, marker )

            else
                ( marker, track.currentNode )
    in
    { options
        | smoothedBend =
            if endPoint.index >= startPoint.index + 2 then
                lookForSmoothBendOption options.bendTrackPointSpacing track startPoint endPoint

            else
                Nothing
    }



-- Bunch of small stuff here I can't be bothered to make modules for.


viewAndEditFilename : Model -> Element Msg
viewAndEditFilename model =
    let
        filename =
            Maybe.withDefault "" model.filename

        trackName =
            Maybe.map .trackName model.track
                |> Maybe.Extra.join
    in
    case model.gpxSource of
        GpxNone ->
            E.none

        _ ->
            column [ Font.size 14 ]
                [ displayName trackName
                , Input.text [ width (px 200) ]
                    { onChange = UserChangedFilename
                    , text = filename
                    , placeholder = Nothing
                    , label = Input.labelHidden "File name"
                    }
                ]


saveButtonIfChanged : Model -> Element Msg
saveButtonIfChanged model =
    case model.undoStack of
        _ :: _ ->
            button
                prettyButtonStyles
                { onPress = Just OutputGPX
                , label = E.text "Save as GPX file to your computer"
                }

        _ ->
            none


outputGPX : Model -> Cmd Msg
outputGPX model =
    let
        gpxString =
            Maybe.map writeGPX model.track
                |> Maybe.withDefault "Sorry, nothing to write."

        outputFilename =
            case model.filename of
                Just filename ->
                    filename
                        ++ (if not (String.endsWith ".GPX" (String.toUpper filename)) then
                                ".gpx"

                            else
                                ""
                           )

                Nothing ->
                    "NOFILENAME"
    in
    Download.string outputFilename "text/gpx" gpxString
