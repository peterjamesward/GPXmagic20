module Main exposing (main)

import About
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
import GeoCodeDecoders exposing (IpInfo)
import GradientSmoother
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import Http
import InsertPoints
import Json.Decode as E exposing (at, decodeValue, field, float)
import Json.Encode
import List.Extra
import Loop
import MapController exposing (..)
import MarkerControls exposing (markerButton, viewTrackControls)
import Maybe.Extra
import MyIP
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
import Track exposing (Track, searchTrackPointFromLonLat, summaryData, updateTrackPointLonLat)
import TrackEditType exposing (TrackEditType(..))
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
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())


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
    , ipInfo : Maybe IpInfo
    , highlightedGraphEdge : Scene
    }


init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    -- We stitch in the OAuth init stuff somehow here.
    let
        ( authData, authCmd ) =
            StravaAuth.init mflags origin navigationKey OAuthMessage
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
      , ipInfo = Nothing
      , highlightedGraphEdge = []
      }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , MapController.createMap MapController.defaultMapInfo
        ]
    )


passFlythroughToContext : Maybe Flythrough -> ViewingContext -> ViewingContext
passFlythroughToContext flight context =
    { context | flythrough = flight }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        ReceivedIpDetails response ->
            let
                ipInfo =
                    MyIP.processIpInfo response
            in
            ( { model | ipInfo = ipInfo }
            , MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
            )

        IpInfoAcknowledged _ ->
            ( model, Cmd.none )

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
            let
                ( newModel, action ) =
                    Maybe.map (processGraphMessage innerMsg model) model.track
                        |> Maybe.withDefault ( model, ActionNoOp )
            in
            processPostUpdateAction newModel action

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

        MapMessage json ->
            -- So we don't need to keep going to the MapController.
            -- These will be Model-domain messages.
            let
                jsonMsg =
                    decodeValue msgDecoder json

                ( lat, lon ) =
                    ( decodeValue (field "lat" float) json
                    , decodeValue (field "lon" float) json
                    )
            in
            case ( jsonMsg, model.track ) of
                ( Ok "click", Just track ) ->
                    --{ 'msg' : 'click'
                    --, 'lat' : e.lat()
                    --, 'lon' : e.lon()
                    --} );
                    case ( lat, lon ) of
                        ( Ok lat1, Ok lon1 ) ->
                            case searchTrackPointFromLonLat ( lon1, lat1 ) track of
                                Just point ->
                                    processPostUpdateAction
                                        model
                                        (PostUpdateActions.ActionFocusMove point)

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                ( Ok "drag", Just track ) ->
                    let
                        newTrack =
                            draggedOnMap json track
                    in
                    processPostUpdateAction
                        model
                        (PostUpdateActions.ActionTrackChanged
                            EditPreservesIndex
                            newTrack
                            "Dragged on map"
                        )

                ( Ok "no node", _ ) ->
                    ( model
                    , Cmd.none
                      --, Delay.after 100 <| MapController.createMap MapController.defaultMapInfo
                    )

                _ ->
                    ( model, Cmd.none )

        DisplayOptionsMessage dispMsg ->
            let
                ( newOptions, action ) =
                    DisplayOptions.update model.displayOptions
                        dispMsg
                        DisplayOptionsMessage

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
                            FlythroughMessage
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
                    StravaTools.update
                        stravaMsg
                        model.stravaOptions
                        model.stravaAuthentication
                        StravaMessage
                        model.track
            in
            processPostUpdateAction
                { model | stravaOptions = newOptions }
                action


draggedOnMap : E.Value -> Track -> Track
draggedOnMap json track =
    -- Map has told us the old and new coordinates of a trackpoint.
    let
        lon1 =
            E.decodeValue (at [ "start", "lng" ] float) json

        lat1 =
            E.decodeValue (at [ "start", "lat" ] float) json

        lon2 =
            E.decodeValue (at [ "end", "lng" ] float) json

        lat2 =
            E.decodeValue (at [ "end", "lat" ] float) json
    in
    case ( ( lon1, lat1 ), ( lon2, lat2 ) ) of
        ( ( Ok startLon, Ok startLat ), ( Ok endLon, Ok endLat ) ) ->
            let
                maybetp =
                    searchTrackPointFromLonLat ( startLon, startLat ) track
            in
            case maybetp of
                Just tp ->
                    { track
                        | trackPoints =
                            List.Extra.updateAt tp.index
                                (updateTrackPointLonLat ( endLon, endLat ) track)
                                track.trackPoints
                    }

                Nothing ->
                    track

        _ ->
            track


processPostUpdateAction : Model -> PostUpdateAction (Cmd Msg) -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    -- I doubt that will ever be true.
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType newTrack undoMsg ) ->
            ( model
                |> addToUndoStack undoMsg
                |> updateTrackInModel newTrack editType
            , Cmd.batch <| ViewPane.makeMapCommands newTrack model.viewPanes
            )

        ( Just track, ActionRerender ) ->
            -- Use this after Undo/Redo to avoid pushing change onto stack.
            ( model
                |> updateTrackInModel track EditNoOp
            , Cmd.batch <| ViewPane.makeMapCommands track model.viewPanes
            )

        ( Just track, ActionWalkGraph ) ->
            -- Graph settings ghave changed. We must get the new route.
            let
                newTrackPoints =
                    Maybe.map Graph.walkTheRoute track.graph
                        |> Maybe.withDefault []

                newTrack =
                    { track | trackPoints = newTrackPoints }

                newModel =
                    { model | track = Just newTrack }
            in
            ( newModel
                |> updateTrackInModel track EditNoOp
            , Cmd.batch <| ViewPane.makeMapCommands newTrack newModel.viewPanes
            )

        ( Just track, ActionPointerMove tp ) ->
            --TODO: Remove duplication from following cases.
            let
                updatedTrack =
                    { track | currentNode = tp }

                bendPreview =
                    { track
                        | trackPoints =
                            Maybe.map .nodes model.bendOptions.smoothedBend
                                |> Maybe.withDefault []
                    }

                nudgePreview =
                    { track | trackPoints = model.nudgeSettings.preview }
            in
            ( { model | track = Just updatedTrack }
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack bendPreview nudgePreview ]
            )

        ( Just track, ActionFocusMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }

                bendPreview =
                    { track
                        | trackPoints =
                            Maybe.map .nodes model.bendOptions.smoothedBend
                                |> Maybe.withDefault []
                    }

                nudgePreview =
                    { track | trackPoints = model.nudgeSettings.preview }
            in
            ( { model
                | track = Just updatedTrack
                , viewPanes = ViewPane.mapOverPanes (updatePointerInLinkedPanes tp) model.viewPanes
              }
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack bendPreview nudgePreview
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

                bendPreview =
                    { track
                        | trackPoints =
                            Maybe.map .nodes model.bendOptions.smoothedBend
                                |> Maybe.withDefault []
                    }

                nudgePreview =
                    { track | trackPoints = model.nudgeSettings.preview }
            in
            ( { model | track = Just updatedTrack }
                |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap updatedTrack bendPreview nudgePreview ]
            )

        ( Just track, ActionRepaintMap ) ->
            ( model
            , Delay.after 50 RepaintMap
            )

        ( Just track, ActionPreview ) ->
            -- We make dummy "Tracks" here for the Map.
            let
                bendPreview =
                    if Accordion.tabIsOpen "Bend smoother classic" model.toolsAccordion then
                        { track
                            | trackPoints =
                                Maybe.map .nodes model.bendOptions.smoothedBend
                                    |> Maybe.withDefault []
                        }

                    else
                        { track | trackPoints = [] }

                nudgePreview =
                    if Accordion.tabIsOpen "Nudge" model.toolsAccordion then
                        { track | trackPoints = model.nudgeSettings.preview }

                    else
                        { track | trackPoints = [] }
            in
            ( model |> renderVaryingSceneElements
            , Cmd.batch
                [ MapController.addMarkersToMap track bendPreview nudgePreview ]
            )

        ( _, ActionCommand a ) ->
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
        , undoStack = []
        , redoStack = []
      }
        |> repeatTrackDerivations
    , Cmd.batch mapCommands
    )


processViewPaneMessage : ViewPaneMessage -> Model -> Track -> ( Model, Cmd Msg )
processViewPaneMessage innerMsg model track =
    let
        ( newPane, postUpdateAction ) =
            ViewPane.update innerMsg model.viewPanes ViewPaneMessage

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


processGraphMessage : Graph.Msg -> Model -> Track -> ( Model, PostUpdateActions.PostUpdateAction msg )
processGraphMessage innerMsg model isTrack =
    let
        ( newGraph, action ) =
            Graph.update innerMsg isTrack.trackPoints isTrack.graph

        newTrack =
            { isTrack | graph = newGraph }

        modelWithUpdatedGraph = { model | track = Just newTrack }
    in
    case action of
        GraphCreated ->
            ( model
            , PostUpdateActions.ActionTrackChanged
                EditNoOp
                newTrack
                "Create Graph"
            )

        GraphOffsetChange ->
            ( modelWithUpdatedGraph
            , PostUpdateActions.ActionNoOp
            )

        GraphRouteChanged ->
            -- Note we must not walk the route as that would remove track points
            -- that we will want to use for alternative routes.
            -- SO we MUST NOT walk the route until we exit graph mode.
            -- That implies that setting the offset should also not walk the route.
            ( modelWithUpdatedGraph
            , PostUpdateActions.ActionPreview
            )

        GraphNoAction ->
            ( model, ActionNoOp )

        GraphRemoved ->
            -- Now we can walk the route safely, applying offset.
            ( model
            , PostUpdateActions.ActionTrackChanged
                EditNoOp
                newTrack
                "Leave Graph mode"
            )

        GraphShowTraversal ->
            ( modelWithUpdatedGraph
            , PostUpdateActions.ActionPreview
            )


updateTrackInModel : Track -> TrackEditType -> Model -> Model
updateTrackInModel newTrack editType model =
    case model.track of
        Just oldTrack ->
            let
                purple =
                    -- Need to get edit region to help the graph assess the changes.
                    Maybe.withDefault oldTrack.currentNode oldTrack.markedNode

                editRegion =
                    ( min oldTrack.currentNode.index purple.index
                    , max oldTrack.currentNode.index purple.index
                    )

                newGraph =
                    Graph.updateWithNewTrack
                        newTrack.graph
                        oldTrack.trackPoints
                        editRegion
                        newTrack.trackPoints
                        editType

                newPointsFromGraph =
                    Maybe.map Graph.walkTheRoute newGraph
                        |> Maybe.withDefault newTrack.trackPoints

                trackWithNewRoute =
                    { newTrack | trackPoints = newPointsFromGraph, graph = newGraph }
            in
            { model | track = Just trackWithNewRoute }
                |> repeatTrackDerivations

        Nothing ->
            model


repeatTrackDerivations : Model -> Model
repeatTrackDerivations model =
    case model.track of
        Just isTrack ->
            let
                newTrack =
                    { isTrack | trackPoints = prepareTrackPoints isTrack.trackPoints }
            in
            { model
                | track = Just newTrack
                , observations = deriveProblems newTrack model.problemOptions
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
                ++ model.highlightedGraphEdge
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

        updatedNudgeSettings =
            let
                settings =
                    model.nudgeSettings
            in
            if
                Accordion.tabIsOpen "Nudge" model.toolsAccordion
                    && Nudge.settingNotZero settings
            then
                Maybe.map (Nudge.previewNudgeNodes model.nudgeSettings) model.track
                    |> Maybe.withDefault settings

            else
                { settings | preview = [] }

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

        graphEdge =
            case model.track of
                Just isTrack ->
                    if Accordion.tabIsOpen "Graph Theory" model.toolsAccordion then
                        Maybe.map Graph.previewTraversal isTrack.graph
                            |> Maybe.withDefault []

                    else
                        []

                Nothing ->
                    []
    in
    { model
        | visibleMarkers = updatedMarkers
        , profileMarkers = updatedProfileMarkers
        , nudgeSettings = updatedNudgeSettings
        , nudgePreview = SceneBuilder.previewNudge updatedNudgeSettings.preview
        , bendOptions = updatedBendOptions
        , bendPreview =
            Maybe.map
                (.nodes >> SceneBuilder.previewBend)
                updatedBendOptions.smoothedBend
                |> Maybe.withDefault []
        , nudgeProfilePreview =
            SceneBuilderProfile.previewNudge
                model.displayOptions
                updatedNudgeSettings.preview
        , straightenOptions = updatedStraightenOptions
        , highlightedGraphEdge = SceneBuilder.showGraphEdge graphEdge
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
                [ topLoadingBar model
                , contentArea model
                ]
        ]
    }


topLoadingBar model =
    row [ spacing 20, padding 10 ]
        [ button
            prettyButtonStyles
            { onPress = Just GpxRequested
            , label = text "Load GPX from your computer"
            }
        , if model.changeCounter == 0 then
            stravaButton
                model.stravaAuthentication
                OAuthMessage

          else
            E.text "Save your work before\nconnecting to Strava"
        , stravaRouteOption
            model.stravaAuthentication
            model.stravaOptions
            StravaMessage
        , viewAndEditFilename model
        , saveButtonIfChanged model
        ]


contentArea model =
    row (width fill :: defaultRowLayout) <|
        [ el [ width fill, alignTop ] <|
            viewAllPanes
                model.viewPanes
                model.displayOptions
                ( model.completeScene, model.completeProfile )
                ViewPaneMessage
        , el [ alignTop ] <|
            column defaultColumnLayout
                [ markerButton model.track MarkerMessage
                , viewTrackControls MarkerMessage model.track
                , undoRedoButtons model
                , Accordion.view
                    (updatedAccordion model model.toolsAccordion toolsAccordion)
                    AccordionMessage
                ]
        ]


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
    if Accordion.tabIsOpen "Fly-through" model.toolsAccordion then
        --if model.flythrough.flythrough /= Nothing then
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
      , content = DisplayOptions.viewDisplayOptions model.displayOptions DisplayOptionsMessage
      , info = DisplayOptions.info
      }
    , { label = "Loop maker"
      , state = Contracted
      , content = Loop.viewLoopTools model.observations.loopiness model.track LoopMsg
      , info = Loop.info
      }
    , { label = "Bend smoother classic"
      , state = Contracted
      , content = BendSmoother.viewBendFixerPane model.bendOptions BendSmoothMessage
      , info = BendSmoother.info
      }
    , { label = "Smooth gradient"
      , state = Contracted
      , content =
            Maybe.map
                (GradientSmoother.viewGradientFixerPane
                    model.gradientOptions
                    GradientMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = GradientSmoother.info
      }
    , { label = "Nudge"
      , state = Contracted
      , content = viewNudgeTools model.nudgeSettings NudgeMessage
      , info = Nudge.info
      }
    , { label = "Straighten"
      , state = Contracted
      , content =
            Maybe.map
                (Straightener.viewStraightenTools
                    model.straightenOptions
                    StraightenMessage
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
                        InsertMessage

                Nothing ->
                    none
      , info = InsertPoints.info
      }
    , { label = "Delete"
      , state = Contracted
      , content = viewDeleteTools model.track DeleteMessage
      , info = DeletePoints.info
      }
    , { label = "Fly-through"
      , state = Contracted
      , content = Flythrough.flythroughControls model.flythrough FlythroughMessage
      , info = Flythrough.info
      }
    , { label = "Filters"
      , state = Contracted
      , content =
            Maybe.map
                (Filters.viewFilterControls model.filterOptions
                    FilterMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = Filters.info
      }
    , { label = "Graph Theory"
      , state = Contracted
      , content =
            model.track
                |> Maybe.map .graph
                |> Maybe.andThen
                    (Just << viewGraphControls GraphMessage)
                |> Maybe.withDefault none
      , info = Graph.info
      }
    , { label = "Route summary"
      , state = Contracted
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
    , { label = "Steep climbs"
      , state = Contracted
      , content =
            Maybe.map
                (TrackObservations.viewSteepClimbs
                    model.problemOptions
                    ProblemMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = TrackObservations.info
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content =
            TrackObservations.viewGradientChanges
                model.problemOptions
                model.observations
                ProblemMessage
      , info = TrackObservations.info
      }
    , { label = "Bend problems"
      , state = Contracted
      , content =
            TrackObservations.viewBearingChanges
                model.problemOptions
                model.observations
                ProblemMessage
      , info = TrackObservations.info
      }
    , { label = "Strava"
      , state = Contracted
      , content =
            Maybe.map
                (StravaTools.viewStravaTab model.stravaOptions StravaMessage)
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
