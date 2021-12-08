module Main exposing (main)

--import StravaTools exposing (stravaRouteOption)

import Accordion exposing (AccordionEntry, AccordionModel, AccordionState(..), view)
import BendSmoother
import BoundingBox3d
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import CurveFormer
import Delay exposing (after)
import DeletePoints exposing (Action(..), viewDeleteTools)
import DisplayOptions exposing (DisplayOptions)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, thumb)
import FeatherIcons
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Filters
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import Flythrough exposing (Flythrough)
import GeoCodeDecoders exposing (IpInfo)
import GpxSource exposing (..)
import GradientLimiter
import GradientSmoother
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import Html.Attributes exposing (id, style)
import Http
import Interpolate
import Json.Decode as D
import Json.Encode as Encode
import Length
import List.Extra
import LoopedTrack
import MarkerControls exposing (markerButton, viewTrackControls)
import Maybe.Extra as Maybe
import MoveAndStretch
import MyIP
import Nudge exposing (NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import OneClickQuickFix exposing (oneClickQuickFix, undoOneClickQuickFix)
import Point3d
import PortController exposing (..)
import PostUpdateActions exposing (PostUpdateAction(..), TrackEditType(..), UndoEntry)
import Quantity
import RotateRoute
import Scene exposing (Scene)
import SceneBuilder exposing (RenderingContext, defaultRenderingContext)
import SceneBuilderProfile
import Straightener
import StravaAuth exposing (getStravaToken, stravaButton)
import SvgPathExtractor
import Task
import Time
import Track exposing (Track, searchTrackPointFromLonLat, summaryData, updateTrackPointLonLat)
import TrackObservations exposing (TrackObservations, deriveProblems)
import TrackPoint exposing (TrackPoint, applyGhanianTransform, prepareTrackPoints)
import TrackSplitter
import Url exposing (Url)
import Utils exposing (combineLists, useIcon)
import ViewPane as ViewPane exposing (ViewPane, ViewPaneAction(..), ViewPaneMessage, is3dVisible, isMapVisible, isProfileVisible, refreshSceneSearcher, setViewPaneSize, updatePointerInLinkedPanes)
import ViewPureStyles exposing (..)
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
    | PortMessage Encode.Value
    | RepaintMap
    | DisplayOptionsMessage DisplayOptions.Msg
    | BendSmoothMessage BendSmoother.Msg
    | LoopMsg LoopedTrack.Msg
    | GradientMessage GradientSmoother.Msg
    | GradientLimiter GradientLimiter.Msg
    | StraightenMessage Straightener.Msg
    | FlythroughMessage Flythrough.Msg
    | FilterMessage Filters.Msg
    | ProblemMessage TrackObservations.Msg
    | InsertMessage Interpolate.Msg
    | SplitterMessage TrackSplitter.Msg
    | UserChangedFilename String
    | OutputGPX
      --| StravaMessage StravaTools.Msg
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | RotateMessage RotateRoute.Msg
    | OneClickQuickFix
    | SvgMessage SvgPathExtractor.Msg
    | EnableMapSketchMode
    | ResizeViews Int
    | StoreSplitterPosition
    | TwoWayDragMsg MoveAndStretch.Msg
    | CurveFormerMsg CurveFormer.Msg
    | ClearMapClickDebounce


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


type Model
    = Model ModelRecord


type alias ModelRecord =
    { filename : Maybe String
    , gpxSource : GpxSource
    , time : Time.Posix
    , zone : Time.Zone
    , completeScene : Scene
    , completeProfile : Scene
    , renderingContext : Maybe RenderingContext
    , viewPanes : List ViewPane
    , track : Maybe Track
    , markerPositionAtLastSceneBuild : Quantity.Quantity Float Length.Meters
    , toolsAccordion : List (AccordionEntry Model Msg)
    , nudgeSettings : NudgeSettings
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , changeCounter : Int
    , displayOptions : DisplayOptions.DisplayOptions
    , bendOptions : BendSmoother.BendOptions
    , observations : TrackObservations
    , gradientOptions : GradientSmoother.Options
    , straightenOptions : Straightener.Options
    , flythrough : Flythrough.Options
    , filterOptions : Filters.Options
    , problemOptions : TrackObservations.Options
    , insertOptions : Interpolate.Options

    --, stravaOptions : StravaTools.Options
    , stravaAuthentication : O.Model
    , ipInfo : Maybe IpInfo
    , gradientLimiter : GradientLimiter.Options
    , rotateOptions : RotateRoute.Options
    , lastMapClick : ( Float, Float )

    --, splitterOptions : TrackSplitter.Options
    , svgData : SvgPathExtractor.Options
    , mapElevations : List Float
    , mapSketchMode : Bool
    , accordionState : Accordion.AccordionModel
    , splitInPixels : Int
    , markerOptions : MarkerControls.Options
    , moveAndStretch : MoveAndStretch.Model
    , curveFormer : CurveFormer.Model
    , mapClickDebounce : Bool
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
      , markerPositionAtLastSceneBuild = Quantity.zero
      , completeScene = []
      , completeProfile = []
      , renderingContext = Nothing
      , viewPanes = ViewPane.viewPanesWhenNoTrack
      , toolsAccordion = []
      , nudgeSettings = defaultNudgeSettings
      , undoStack = []
      , redoStack = []
      , changeCounter = 0
      , displayOptions = DisplayOptions.defaultDisplayOptions
      , bendOptions = BendSmoother.defaultOptions
      , observations = TrackObservations.defaultObservations
      , gradientOptions = GradientSmoother.defaultOptions
      , straightenOptions = Straightener.defaultOptions
      , flythrough = Flythrough.defaultOptions
      , filterOptions = Filters.defaultOptions
      , problemOptions = TrackObservations.defaultOptions
      , insertOptions = Interpolate.defaultOptions

      --, stravaOptions = StravaTools.defaultOptions
      , stravaAuthentication = authData
      , ipInfo = Nothing
      , gradientLimiter = GradientLimiter.defaultOptions
      , rotateOptions = RotateRoute.defaultOptions
      , lastMapClick = ( 0.0, 0.0 )

      --, splitterOptions = TrackSplitter.defaultOptions
      , svgData = SvgPathExtractor.empty
      , mapElevations = []
      , mapSketchMode = False
      , accordionState = Accordion.defaultState
      , splitInPixels = 800
      , markerOptions = MarkerControls.defaultOptions
      , moveAndStretch = MoveAndStretch.defaultModel
      , curveFormer = CurveFormer.defaultModel
      , mapClickDebounce = False
      }
        -- Just make sure the Accordion reflects all the other state.
        |> (\record -> { record | toolsAccordion = toolsAccordion (Model record) })
        |> Model
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , PortController.storageGetItem "accordion"
        , PortController.storageGetItem "display"

        --, PortController.storageGetItem "panes"
        ]
    )


passFlythroughToContext : Maybe Flythrough -> ViewingContext -> ViewingContext
passFlythroughToContext flight context =
    { context | flythrough = flight }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        AdjustTimeZone newZone ->
            ( Model { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        ReceivedIpDetails response ->
            let
                ipInfo =
                    MyIP.processIpInfo response

                mapInfoWithLocation =
                    case ipInfo of
                        Just ip ->
                            { defaultMapInfo
                                | centreLon = ip.longitude
                                , centreLat = ip.latitude
                                , mapZoom = 10.0
                            }

                        Nothing ->
                            defaultMapInfo
            in
            ( Model { model | ipInfo = ipInfo }
            , Cmd.batch
                [ MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                , PortController.createMap mapInfoWithLocation
                ]
            )

        IpInfoAcknowledged _ ->
            ( Model model, Cmd.none )

        Tick newTime ->
            let
                flythrough =
                    model.flythrough

                updatedFlythrough =
                    Flythrough.advanceFlythrough
                        newTime
                        { flythrough | modelTime = newTime }
            in
            ( Model
                { model
                    | time = newTime
                    , flythrough = updatedFlythrough
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
            ( Model model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( Model { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            processGpxLoaded content <| Model model

        ViewPaneMessage innerMsg ->
            Maybe.map (processViewPaneMessage innerMsg (Model model)) model.track
                |> Maybe.withDefault ( Model model, Cmd.none )

        RepaintMap ->
            ( Model model, refreshMap )

        GraphMessage innerMsg ->
            let
                ( newModel, action ) =
                    Maybe.map (processGraphMessage innerMsg model) model.track
                        |> Maybe.withDefault ( model, ActionNoOp )
            in
            processPostUpdateAction newModel action

        AccordionMessage accordionMsg ->
            let
                ( newState, newAccordion ) =
                    Accordion.update accordionMsg model.accordionState model.toolsAccordion

                newModel =
                    { model
                        | toolsAccordion = newAccordion
                        , accordionState = newState
                    }

                -- Cheeky hack to add storage message in here.
                -- We ask for a preview in case user has opened Bend Smoother, Nudge, say.
                ( finalModel, cmds ) =
                    processPostUpdateAction
                        newModel
                        PostUpdateActions.ActionPreview
            in
            ( finalModel
            , Cmd.batch
                [ PortController.storageSetItem "accordion" (Accordion.storedState newState newAccordion)
                , cmds
                ]
            )

        MarkerMessage markerMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map (MarkerControls.update markerMsg model.markerOptions MarkerMessage) model.track
                        |> Maybe.withDefault ( model.markerOptions, ActionNoOp )

                newModel =
                    { model | markerOptions = newOptions }
            in
            processPostUpdateAction newModel action

        NudgeMessage nudgeMsg ->
            case model.track of
                Just track ->
                    let
                        ( newSetttings, action ) =
                            Nudge.update nudgeMsg
                                model.displayOptions.imperialMeasure
                                model.nudgeSettings
                                track
                    in
                    processPostUpdateAction
                        { model | nudgeSettings = newSetttings }
                        action

                Nothing ->
                    ( Model model, Cmd.none )

        InsertMessage insertMsg ->
            let
                ( newSettings, action ) =
                    Maybe.map
                        (Interpolate.update
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
                    Maybe.map (DeletePoints.update model.displayOptions.imperialMeasure deleteMsg) model.track
                        |> Maybe.withDefault ActionNoOp
            in
            processPostUpdateAction model action

        --Delegate wrapped OAuthmessages. Be bowled over if this works first time. Or fiftieth.
        --Maybe look after to see if there is yet a token. Easy way to know.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData
            in
            ( Model { model | stravaAuthentication = newAuthData }
            , Cmd.map OAuthMessage authCmd
            )

        ClearMapClickDebounce ->
            ( Model { model | mapClickDebounce = False }
            , Cmd.none
            )

        PortMessage json ->
            processPortMessage model json

        DisplayOptionsMessage dispMsg ->
            let
                newOptions =
                    DisplayOptions.update
                        model.displayOptions
                        dispMsg
                        DisplayOptionsMessage
            in
            ( { model | displayOptions = newOptions }
                |> refreshAccordion
                |> composeScene
                |> Model
            , PortController.storageSetItem "display" (DisplayOptions.encodeOptions newOptions)
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
                    Maybe.map (LoopedTrack.update loopMsg model.observations.loopiness) model.track
                        |> Maybe.withDefault ( model.observations.loopiness, ActionNoOp )

                oldObs =
                    model.observations

                newObs =
                    { oldObs | loopiness = newOptions }
            in
            processPostUpdateAction
                { model | observations = newObs }
                action

        GradientLimiter limitMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map (GradientLimiter.update limitMsg model.gradientLimiter)
                        model.track
                        |> Maybe.withDefault ( model.gradientLimiter, ActionNoOp )
            in
            processPostUpdateAction
                { model | gradientLimiter = newOptions }
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

        RotateMessage rotate ->
            let
                ( newOptions, action ) =
                    Maybe.map (RotateRoute.update rotate model.rotateOptions model.lastMapClick)
                        model.track
                        |> Maybe.withDefault ( model.rotateOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | rotateOptions = newOptions }
                action

        --SplitterMessage splitter ->
        --    let
        --        ( newOptions, action ) =
        --            TrackSplitter.update
        --                splitter
        --                model.splitterOptions
        --                model.observations
        --                model.track
        --                SplitterMessage
        --
        --        newModel =
        --            { model
        --                | splitterOptions = newOptions
        --            }
        --    in
        --    processPostUpdateAction newModel action
        --
        ProblemMessage probMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (TrackObservations.update
                            probMsg
                            model.problemOptions
                            model.bendOptions.segments
                        )
                        model.track
                        |> Maybe.withDefault ( model.problemOptions, ActionNoOp )
            in
            processPostUpdateAction
                { model | problemOptions = newOptions }
                action

        UserChangedFilename txt ->
            ( Model { model | filename = Just txt }
            , Cmd.none
            )

        OutputGPX ->
            ( Model { model | changeCounter = 0 }
            , outputGPX model
            )

        OneClickQuickFix ->
            case model.track of
                Just track ->
                    let
                        undoEntry : UndoEntry
                        undoEntry =
                            { label = "One-click Quick-Fix"
                            , editFunction = oneClickQuickFix
                            , undoFunction = always (undoOneClickQuickFix track)
                            , newOrange = 0
                            , newPurple = Nothing
                            , oldOrange = 0
                            , oldPurple = Nothing
                            }

                        ( Model newModel, cmds ) =
                            processPostUpdateAction
                                model
                                (PostUpdateActions.ActionTrackChanged
                                    PostUpdateActions.EditPreservesNodePosition
                                    undoEntry
                                )
                    in
                    ( Model newModel
                    , Cmd.batch
                        [ outputGPX newModel
                        , cmds
                        ]
                    )

                Nothing ->
                    ( Model model, Cmd.none )

        --StravaMessage stravaMsg ->
        --    let
        --        ( newOptions, action ) =
        --            StravaTools.update
        --                stravaMsg
        --                model.stravaOptions
        --                model.stravaAuthentication
        --                StravaMessage
        --                model.track
        --    in
        --    processPostUpdateAction
        --        { model | stravaOptions = newOptions }
        --        action
        SvgMessage svgMsg ->
            let
                ( newData, cmd ) =
                    SvgPathExtractor.update
                        svgMsg
                        model.svgData
                        SvgMessage
            in
            case newData.track of
                Just isTrack ->
                    isTrack |> applyTrack (Model model)

                Nothing ->
                    ( Model { model | svgData = newData }
                    , cmd
                    )

        EnableMapSketchMode ->
            let
                ( lon, lat ) =
                    case model.ipInfo of
                        Just info ->
                            ( info.longitude, info.latitude )

                        Nothing ->
                            ( 0.0, 0.0 )
            in
            case model.mapSketchMode of
                False ->
                    ( Model { model | mapSketchMode = True }
                    , Cmd.batch
                        [ PortController.prepareSketchMap ( lon, lat )
                        , Delay.after 500 RepaintMap
                        ]
                    )

                True ->
                    ( Model { model | mapSketchMode = False }
                    , PortController.exitSketchMode
                    )

        ResizeViews newPosition ->
            ( Model
                { model
                    | splitInPixels = newPosition
                    , viewPanes = ViewPane.mapOverPanes (setViewPaneSize newPosition) model.viewPanes
                }
            , Cmd.batch
                [ Delay.after 50 RepaintMap
                , PortController.storageSetItem "splitter" (Encode.int model.splitInPixels)
                ]
            )

        StoreSplitterPosition ->
            ( Model model
            , PortController.storageSetItem "splitter" (Encode.int model.splitInPixels)
            )

        TwoWayDragMsg dragMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (MoveAndStretch.update
                            dragMsg
                            model.moveAndStretch
                            TwoWayDragMsg
                        )
                        model.track
                        |> Maybe.withDefault ( model.moveAndStretch, ActionNoOp )
            in
            processPostUpdateAction
                { model | moveAndStretch = newOptions }
                action

        CurveFormerMsg curveMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (CurveFormer.update
                            curveMsg
                            model.curveFormer
                            CurveFormerMsg
                        )
                        model.track
                        |> Maybe.withDefault ( model.curveFormer, ActionNoOp )
            in
            processPostUpdateAction
                { model | curveFormer = newOptions }
                action

        _ ->
            ( Model model, Cmd.none )


draggedOnMap : Encode.Value -> Track -> Maybe Track
draggedOnMap json track =
    -- Map has told us the old and new coordinates of a trackpoint.
    -- Return Nothing if drag did not change track.
    let
        lon1 =
            D.decodeValue (D.at [ "start", "lng" ] D.float) json

        lat1 =
            D.decodeValue (D.at [ "start", "lat" ] D.float) json

        lon2 =
            D.decodeValue (D.at [ "end", "lng" ] D.float) json

        lat2 =
            D.decodeValue (D.at [ "end", "lat" ] D.float) json
    in
    if lon1 == lon2 && lat1 == lat2 then
        Nothing

    else
        case ( ( lon1, lat1 ), ( lon2, lat2 ) ) of
            ( ( Ok startLon, Ok startLat ), ( Ok endLon, Ok endLat ) ) ->
                let
                    maybetp =
                        searchTrackPointFromLonLat ( startLon, startLat ) track
                in
                case maybetp of
                    Just tp ->
                        Just
                            { track
                                | trackPoints =
                                    List.Extra.updateAt tp.index
                                        (updateTrackPointLonLat ( endLon, endLat ) track)
                                        track.trackPoints
                            }

                    Nothing ->
                        Nothing

            _ ->
                Nothing


processPostUpdateAction : ModelRecord -> PostUpdateAction Track (Cmd Msg) -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    let
        fromModel (Model m) =
            -- I dislike stuff like this.
            m
    in
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType undoEntry ) ->
            let
                results =
                    undoEntry.editFunction track

                ( prefix, changed, suffix ) =
                    ( results.before, results.edited, results.after )

                newPoints =
                    (prefix ++ changed ++ suffix) |> prepareTrackPoints

                newTrack =
                    { track
                        | trackPoints = newPoints
                        , currentNode =
                            newPoints
                                |> List.Extra.getAt undoEntry.newOrange
                                |> Maybe.withDefault track.currentNode
                        , markedNode =
                            case undoEntry.newPurple of
                                Just p ->
                                    newPoints |> List.Extra.getAt p

                                Nothing ->
                                    Nothing
                        , earthReferenceCoordinates = results.earthReferenceCoordinates
                    }

                newModel =
                    { model | track = Just newTrack }
                        |> addToUndoStack undoEntry
            in
            processPostUpdateAction newModel ActionRerender

        ( Just track, ActionRerender ) ->
            -- Use this after Undo/Redo to avoid pushing change onto stack.
            let
                polishedModel =
                    model
                        |> reflectNewTrackViaGraph track EditNoOp
                        |> repeatTrackDerivations
                        |> refreshSceneSearchers
                        |> refreshAccordion
                        |> composeScene
            in
            ( Model polishedModel
            , Cmd.batch
                [ ViewPane.makeMapCommands track polishedModel.viewPanes (getMapPreviews polishedModel)
                , Delay.after 100 RepaintMap
                ]
            )

        ( Just track, ActionWalkGraph ) ->
            -- Graph settings have changed. We must get the new route.
            let
                newTrackPoints =
                    Maybe.map Graph.walkTheRoute track.graph
                        |> Maybe.withDefault []

                newTrack =
                    { track | trackPoints = newTrackPoints }

                newModel =
                    { model | track = Just newTrack }
            in
            processPostUpdateAction newModel ActionRerender

        ( Just track, ActionPointerMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }

                newModel =
                    { model | track = Just updatedTrack }
            in
            processPostUpdateAction newModel ActionRerender

        ( Just track, ActionFocusMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }

                newModel =
                    { model
                        | track = Just updatedTrack
                        , viewPanes = ViewPane.mapOverPanes (updatePointerInLinkedPanes tp) model.viewPanes
                    }

                ( finalModel, cmd ) =
                    processPostUpdateAction newModel ActionRerender

                refocusMapCmd =
                    if ViewPane.mapPaneIsLinked model.viewPanes then
                        PortController.centreMapOnCurrent track

                    else
                        Cmd.none
            in
            ( finalModel
            , Cmd.batch [ refocusMapCmd, cmd ]
            )

        ( Just track, ActionMarkerMove maybeTp ) ->
            let
                updatedTrack =
                    { track | markedNode = maybeTp }

                newModel =
                    { model | track = Just updatedTrack }
            in
            processPostUpdateAction newModel ActionRerender

        ( Just track, ActionRepaintMap ) ->
            ( Model model
            , Delay.after 50 RepaintMap
            )

        ( Just track, ActionToggleMapDragging isDragging ) ->
            ( Model model
            , PortController.toggleDragging isDragging track
            )

        ( Just track, ActionFetchMapElevations ) ->
            ( Model model
            , PortController.requestElevations
            )

        ( Just track, ActionPreview ) ->
            processPostUpdateAction model ActionRerender

        ( _, ActionCommand a ) ->
            ( Model model, a )

        ( _, ActionNewRoute content source ) ->
            let
                ( Model newModel, cmds ) =
                    { model | gpxSource = source }
                        |> Model
                        |> processGpxLoaded content
            in
            processPostUpdateAction newModel ActionRerender

        _ ->
            ( Model model, Cmd.none )


getMapPreviews model =
    case model.track of
        Nothing ->
            []

        Just track ->
            let
                emptyTrack =
                    { track
                        | trackPoints = [ track.currentNode ]
                        , markedNode = Nothing
                    }

                previews =
                    model.toolsAccordion
                        |> Accordion.openTabs
                        |> List.map
                            (\tab ->
                                case tab.previewMap of
                                    Just fn ->
                                        Just (fn track)

                                    Nothing ->
                                        Nothing
                            )
                        |> List.filterMap identity

                noPreviews =
                    -- !!
                    model.toolsAccordion
                        |> Accordion.closedTabs
                        |> List.map
                            (\tab ->
                                case tab.previewMap of
                                    Just fn ->
                                        Just (fn emptyTrack)

                                    Nothing ->
                                        Nothing
                            )
                        |> List.filterMap identity
            in
            previews ++ noPreviews


processGpxLoaded : String -> Model -> ( Model, Cmd Msg )
processGpxLoaded content (Model model) =
    case Track.trackFromGpx content of
        Just track ->
            applyTrack
                (Model
                    { model
                        | viewPanes =
                            -- Force third person view on first file load.
                            if model.track == Nothing then
                                ViewPane.viewPanesWithTrack

                            else
                                model.viewPanes
                    }
                )
                track

        Nothing ->
            ( Model model, Cmd.none )


applyTrack : Model -> Track -> ( Model, Cmd Msg )
applyTrack (Model model) track =
    let
        ( newViewPanes, mapCommands ) =
            ( ViewPane.mapOverPanes (ViewPane.resetAllViews track) model.viewPanes
            , Cmd.batch
                [ ViewPane.initialiseMap track model.viewPanes
                , PortController.storageGetItem "panes"
                , PortController.storageGetItem "splitter"
                , Delay.after 100 RepaintMap
                ]
            )
    in
    ( { model
        | track = Just track
        , renderingContext = Just defaultRenderingContext
        , viewPanes = newViewPanes
        , gpxSource = GpxLocalFile
        , undoStack = []
        , redoStack = []
        , changeCounter = 0
        , displayOptions = DisplayOptions.adjustDetail model.displayOptions (List.length track.trackPoints)
      }
        |> repeatTrackDerivations
        |> Model
    , mapCommands
    )


refreshSceneSearchers : ModelRecord -> ModelRecord
refreshSceneSearchers model =
    case model.track of
        Just isTrack ->
            { model
                | viewPanes =
                    ViewPane.mapOverAllContexts
                        (ViewPane.refreshSceneSearcher isTrack)
                        model.viewPanes
            }

        Nothing ->
            model


processViewPaneMessage : ViewPaneMessage -> Model -> Track -> ( Model, Cmd Msg )
processViewPaneMessage innerMsg (Model model) track =
    let
        ( newPane, postUpdateAction ) =
            ViewPane.update innerMsg model.displayOptions model.viewPanes ViewPaneMessage

        updatedViewPanes =
            ViewPane.updateViewPanes newPane model.viewPanes

        updatedModel =
            { model | viewPanes = updatedViewPanes }
    in
    case postUpdateAction of
        ViewPane.ImageAction innerAction ->
            processPostUpdateAction updatedModel innerAction

        ViewPane.PaneLayoutChange f ->
            let
                updatedPanes =
                    ViewPane.mapOverPanes
                        (f >> ViewPane.setViewPaneSize model.splitInPixels)
                        updatedModel.viewPanes

                modelWithNewPanes =
                    { updatedModel | viewPanes = updatedPanes }

                ( finalModel, commands ) =
                    processPostUpdateAction modelWithNewPanes ActionRerender
            in
            ( finalModel
            , Cmd.batch
                [ PortController.storageSetItem "panes" (ViewPane.storePaneLayout updatedPanes)
                , commands
                ]
            )

        ViewPane.PaneNoOp ->
            ( Model updatedModel, Cmd.none )


processGraphMessage :
    Graph.Msg
    -> ModelRecord
    -> Track
    -> ( ModelRecord, PostUpdateActions.PostUpdateAction trck msg )
processGraphMessage innerMsg model isTrack =
    ( model, ActionNoOp )



--let
--    ( newGraph, action ) =
--        Graph.update innerMsg
--            isTrack.trackPoints
--            isTrack.currentNode
--            isTrack.markedNode
--            isTrack.graph
--
--    newTrack =
--        { isTrack | graph = newGraph }
--
--    modelWithUpdatedGraph =
--        { model | track = Just newTrack }
--in
--case action of
--    GraphCreated ->
--        ( model
--        , PostUpdateActions.ActionTrackChanged
--            EditNoOp
--            newTrack
--            "Create Graph"
--        )
--
--    GraphOffsetChange ->
--        ( modelWithUpdatedGraph
--        , PostUpdateActions.ActionNoOp
--        )
--
--    GraphRouteChanged ->
--        -- Note we must not walk the route as that would remove track points
--        -- that we will want to use for alternative routes.
--        -- SO we MUST NOT walk the route until we exit graph mode.
--        -- That implies that setting the offset should also not walk the route.
--        ( modelWithUpdatedGraph
--        , PostUpdateActions.ActionPreview
--        )
--
--    GraphNoAction ->
--        ( model, ActionNoOp )
--
--    GraphRemoved ->
--        -- Now we can walk the route safely, applying offset.
--        let
--            trackFromGraph =
--                { isTrack
--                    | trackPoints =
--                        Maybe.map Graph.publishUserRoute newGraph
--                            |> Maybe.withDefault []
--                    , graph = Nothing
--                }
--
--            modelFromGraph =
--                { model | track = Just trackFromGraph }
--        in
--        ( model
--        , PostUpdateActions.ActionTrackChanged
--            EditNoOp
--            trackFromGraph
--            "Leave Graph mode"
--        )
--
--    GraphShowTraversal ->
--        ( modelWithUpdatedGraph
--        , PostUpdateActions.ActionPreview
--        )


reflectNewTrackViaGraph : Track -> TrackEditType -> ModelRecord -> ModelRecord
reflectNewTrackViaGraph newTrack editType model =
    model



-- We need this in case we have a Graph in effect, and the edits need to be
-- reflected in the graph and the final new route dervied from the graph again.
-- If there's no graph, it's basically a noop.
--case model.track of
--    Just oldTrack ->
--        let
--            orange =
--                oldTrack.currentNode
--
--            purple =
--                Maybe.withDefault oldTrack.currentNode oldTrack.markedNode
--
--            editRegion =
--                ( min orange.index purple.index
--                , max orange.index purple.index
--                )
--
--            changeInLength =
--                List.length newTrack.trackPoints - List.length oldTrack.trackPoints
--
--            newOrange =
--                Maybe.withDefault orange <|
--                    if orange.index <= purple.index then
--                        List.Extra.getAt orange.index newTrack.trackPoints
--
--                    else
--                        List.Extra.getAt (orange.index + changeInLength) newTrack.trackPoints
--
--            newPurple =
--                case oldTrack.markedNode of
--                    Just mark ->
--                        if mark.index <= orange.index then
--                            List.Extra.getAt mark.index newTrack.trackPoints
--
--                        else
--                            List.Extra.getAt (mark.index + changeInLength) newTrack.trackPoints
--
--                    Nothing ->
--                        Nothing
--
--            newGraph =
--                Graph.updateWithNewTrack
--                    newTrack.graph
--                    oldTrack.trackPoints
--                    -- Pre-edit baseline points.
--                    editRegion
--                    -- Where the markers were.
--                    newTrack.trackPoints
--                    -- Post-edit track points.
--                    editType
--
--            newPointsFromGraph =
--                Maybe.map Graph.walkTheRoute newGraph
--                    |> Maybe.withDefault newTrack.trackPoints
--
--            trackWithNewRoute =
--                case newTrack.graph of
--                    Just graph ->
--                        { newTrack
--                            | trackPoints = newPointsFromGraph
--                            , currentNode = newOrange
--                            , markedNode = newPurple
--                            , graph = newGraph
--                        }
--
--                    Nothing ->
--                        newTrack
--        in
--        { model | track = Just trackWithNewRoute }
--            |> repeatTrackDerivations
--
--    Nothing ->
--        model


repeatTrackDerivations : ModelRecord -> ModelRecord
repeatTrackDerivations model =
    case model.track of
        Just isTrack ->
            let
                earthTrack =
                    -- Make sure all new points have lat & lon.
                    isTrack
                        |> Track.removeGhanianTransform
                        |> applyGhanianTransform isTrack.earthReferenceCoordinates
                        |> prepareTrackPoints

                replacePointer pointerNode =
                    earthTrack
                        |> List.Extra.getAt pointerNode.index
                        |> Maybe.withDefault pointerNode

                newOrange =
                    replacePointer isTrack.currentNode

                newPurple =
                    Maybe.map replacePointer isTrack.markedNode

                newBox =
                    BoundingBox3d.hullOfN .xyz earthTrack
                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

                newTrack =
                    { isTrack
                        | trackPoints = earthTrack
                        , currentNode = newOrange
                        , markedNode = newPurple
                        , box = newBox
                        , spatialIndex = Track.buildSpatialIndex earthTrack newBox
                    }
            in
            { model
                | track = Just newTrack
                , observations = deriveProblems newTrack model.problemOptions
            }

        Nothing ->
            model


composeScene : ModelRecord -> ModelRecord
composeScene model =
    case model.track of
        Nothing ->
            model

        Just track ->
            let
                ( xSize, ySize, _ ) =
                    BoundingBox3d.dimensions track.box

                threshold =
                    Quantity.max xSize ySize
                        |> Quantity.multiplyBy (0.5 ^ (1 + model.displayOptions.levelOfDetailThreshold))

                reducedTrack =
                    if model.displayOptions.levelOfDetailThreshold > 0.0 then
                        Track.makeReducedTrack track threshold

                    else
                        track
            in
            { model
                | completeScene =
                    combineLists
                        [ renderVarying3dSceneElements model reducedTrack
                        , renderTrack3dSceneElements model reducedTrack
                        ]
                , completeProfile =
                    combineLists
                        [ renderVaryingProfileSceneElements model reducedTrack
                        , renderTrackProfileSceneElements model reducedTrack
                        ]
            }


renderVaryingProfileSceneElements : ModelRecord -> Track -> Scene
renderVaryingProfileSceneElements model isTrack =
    let
        previews =
            model.toolsAccordion
                |> Accordion.openTabs
                |> List.map
                    (\tab ->
                        case tab.previewProfile of
                            Just fn ->
                                Just (fn isTrack)

                            Nothing ->
                                Nothing
                    )
                |> List.filterMap identity
                |> Utils.combineLists
    in
    [ SceneBuilderProfile.renderMarkers model.displayOptions isTrack
    , previews
    ]
        |> Utils.combineLists


renderVarying3dSceneElements : ModelRecord -> Track -> Scene
renderVarying3dSceneElements model isTrack =
    let
        previews =
            model.toolsAccordion
                |> Accordion.openTabs
                |> List.map
                    (\tab ->
                        case tab.preview3D of
                            Just fn ->
                                Just (fn isTrack)

                            Nothing ->
                                Nothing
                    )
                |> List.filterMap identity
                |> Utils.combineLists
    in
    [ SceneBuilder.renderMarkers isTrack
    , if model.displayOptions.showRenderingLimit then
        SceneBuilder.renderMRLimits isTrack

      else
        []
    , previews

    --updatedMoveAndStretchSettings =
    --    let
    --        settings =
    --            latestModel.moveAndStretch
    --    in
    --    if
    --        Accordion.tabIsOpen MoveAndStretch.toolLabel latestModel.toolsAccordion
    --            && MoveAndStretch.settingNotZero latestModel.moveAndStretch
    --    then
    --            let
    --                whiteMarker =
    --                    SceneBuilder.renderMarkers
    --Nothing isTrack
    --        Maybe.map (MoveAndStretch.preview latestModel.moveAndStretch) latestModel.track
    --            |> Maybe.withDefault latestModel.moveAndStretch
    --
    --    else
    --        { settings | preview = [] }
    --updatedBendOptions =
    --    if Accordion.tabIsOpen BendSmoother.toolLabel latestModel.toolsAccordion then
    --        Maybe.map (tryBendSmoother latestModel.bendOptions) latestModel.track
    --            |> Maybe.withDefault latestModel.bendOptions
    --
    --    else
    --        BendSmoother.defaultOptions
    --updatedStravaOptions =
    --    -- TODO: ?? Move pointers to discovered paste start and end ??
    --    let
    --        options =
    --            latestModel.stravaOptions
    --    in
    --    if Accordion.tabIsOpen StravaTools.toolLabel latestModel.toolsAccordion then
    --        { options
    --            | preview =
    --                Maybe.map (StravaTools.preview options) latestModel.track
    --                    |> Maybe.withDefault []
    --        }
    --
    --    else
    --        { options | preview = [] }
    --updatedStraightenOptions =
    --    let
    --        options =
    --            latestModel.straightenOptions
    --    in
    --    if Accordion.tabIsOpen Straightener.toolLabel latestModel.toolsAccordion then
    --        Maybe.map (Straightener.lookForSimplifications options) latestModel.track
    --            |> Maybe.withDefault options
    --
    --    else
    --        options
    --graphEdge =
    --    case latestModel.track of
    --        Just isTrack ->
    --            if Accordion.tabIsOpen Graph.toolLabel latestModel.toolsAccordion then
    --                Maybe.map Graph.previewTraversal isTrack.graph
    --                    |> Maybe.withDefault []
    --
    --            else
    --                []
    --
    --        Nothing ->
    --            []
    ]
        |> Utils.combineLists


renderTrackProfileSceneElements : ModelRecord -> Track -> Scene
renderTrackProfileSceneElements model isTrack =
    if isProfileVisible model.viewPanes then
        SceneBuilderProfile.renderTrack model.displayOptions isTrack

    else
        []


renderTrack3dSceneElements : ModelRecord -> Track -> Scene
renderTrack3dSceneElements model isTrack =
    let
        updatedScene =
            if is3dVisible model.viewPanes then
                if model.displayOptions.terrainOn then
                    SceneBuilder.renderTerrain model.displayOptions isTrack

                else
                    SceneBuilder.renderTrack model.displayOptions isTrack

            else
                []
    in
    updatedScene


view : Model -> Browser.Document Msg
view (Model model) =
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
                , footer model
                ]
        ]
    }


topLoadingBar model =
    let
        loadGpxButton =
            button
                prettyButtonStyles
                { onPress = Just GpxRequested
                , label = text "Load GPX from your computer"
                }
    in
    row [ spacing 20, padding 10 ]
        [ loadGpxButton
        , if model.changeCounter == 0 then
            stravaButton
                model.stravaAuthentication
                OAuthMessage

          else
            E.text "Save your work before\nconnecting to Strava"

        --, stravaRouteOption
        --    model.stravaAuthentication
        --    model.stravaOptions
        --    StravaMessage
        , viewAndEditFilename model
        , case model.track of
            Just _ ->
                button
                    prettyButtonStyles
                    { onPress = Just OneClickQuickFix
                    , label = E.text "One-click Quick-fix!"
                    }

            Nothing ->
                none
        , saveButtonIfChanged model
        , buyMeACoffeeButton
        ]


footer : ModelRecord -> Element Msg
footer model =
    -- Rather hacky addition of secondary map here.
    column [ spacing 20, padding 10 ]
        [ row [ spacing 20, padding 10 ]
            [ SvgPathExtractor.view SvgMessage
            , mapSketchEnable model
            ]
        , conditionallyVisible model.mapSketchMode <|
            el
                [ width <| px <| model.splitInPixels - 20
                , height <| px <| model.splitInPixels - 20
                , alignLeft
                , alignTop
                , Border.width 1
                , htmlAttribute (id "sketchMap")
                ]
                none
        ]


mapSketchEnable : ModelRecord -> Element Msg
mapSketchEnable model =
    if model.mapSketchMode then
        button
            prettyButtonStyles
            { onPress = Just EnableMapSketchMode
            , label = text "Lift into GPXmagic"
            }

    else
        button
            prettyButtonStyles
            { onPress = Just EnableMapSketchMode
            , label = text "Draw a route on the map"
            }


buyMeACoffeeButton =
    newTabLink
        [ alignRight ]
        { url = "https://www.buymeacoffee.com/Peterward"
        , label =
            image [ height (px 60), width (px 217) ]
                { src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                , description = "Buy Me A Coffee"
                }
        }


minimumLeftPane =
    600


maximumLeftPane =
    1400


contentArea : ModelRecord -> Element Msg
contentArea model =
    let
        leftPane =
            column
                [ width fill, alignTop ]
                [ viewAllPanes
                    model.viewPanes
                    model
                    ( model.completeScene, model.completeProfile, model.completeScene )
                    ViewPaneMessage
                , viewTrackControls MarkerMessage model.track
                ]

        rightPane =
            if model.track /= Nothing then
                column [ spacing 5, padding 5, alignTop ]
                    [ markerButton model.displayOptions.imperialMeasure model.track MarkerMessage
                    , viewTrackControls MarkerMessage model.track
                    , undoRedoButtons model
                    , Accordion.view
                        model.accordionState
                        model.toolsAccordion
                        AccordionMessage
                        (Model model)
                    ]

            else
                Utils.showLogos

        splitter =
            Input.slider
                [ height <| px 30
                , width <| px <| maximumLeftPane - minimumLeftPane
                , behindContent <|
                    -- Slider track
                    el
                        [ width fill
                        , height <| px 5
                        , centerY
                        , centerX
                        ]
                        none
                ]
                { onChange = ResizeViews << round
                , label =
                    Input.labelHidden "Splitter"
                , min = toFloat minimumLeftPane
                , max = toFloat maximumLeftPane
                , step = Just 1
                , value = toFloat <| model.splitInPixels
                , thumb = customThumb
                }

        customThumb =
            Input.thumb
                [ width (px 50)
                , height (px 24)
                , Border.rounded 1
                , Border.width 1
                , Border.color (rgb 0.5 0.5 0.5)
                , Background.color FlatColors.BritishPalette.seabrook
                , Font.color FlatColors.ChinesePalette.white
                , inFront <| row [ centerX ] [ useIcon FeatherIcons.chevronsLeft, useIcon FeatherIcons.chevronsRight ]
                ]

        verticalBar =
            if model.track /= Nothing then
                el
                    [ width <| px 4
                    , height fill
                    , Background.color FlatColors.BritishPalette.seabrook
                    ]
                    none

            else
                none

        showSplitControl =
            if model.track /= Nothing then
                splitter

            else
                none
    in
    column [ width fill, padding 5 ]
        [ row []
            [ el [ width <| px minimumLeftPane ] none
            , showSplitControl
            ]
        , row [ width fill, spacing 5, padding 5 ]
            [ el [ width <| px model.splitInPixels, alignTop ] leftPane
            , verticalBar
            , el [ alignTop, width fill ] rightPane
            ]
        , row []
            [ el [ width <| px minimumLeftPane ] none
            , showSplitControl
            ]
        ]


viewAllPanes :
    List ViewPane
    -> { m | displayOptions : DisplayOptions, ipInfo : Maybe IpInfo }
    -> ( Scene, Scene, Scene )
    -> (ViewPaneMessage -> Msg)
    -> Element Msg
viewAllPanes panes model ( scene, profile, plan ) wrapper =
    wrappedRow [ width fill, spacing 10 ] <|
        List.map
            (ViewPane.view ( scene, profile, plan ) model wrapper)
            panes


refreshAccordion : ModelRecord -> ModelRecord
refreshAccordion model =
    let
        preserveCurrentState currentVersion definedVersion =
            { definedVersion | state = currentVersion.state }
    in
    { model
        | toolsAccordion =
            List.map2 preserveCurrentState
                model.toolsAccordion
                (toolsAccordion (Model model))
    }


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    if Accordion.tabIsOpen Flythrough.toolLabel model.toolsAccordion then
        --if model.flythrough.flythrough /= Nothing then
        Sub.batch
            [ PortController.messageReceiver PortMessage
            , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
            , Time.every 50 Tick
            , MarkerControls.subscription model.markerOptions MarkerMessage
            ]

    else
        Sub.batch
            [ PortController.messageReceiver PortMessage
            , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
            , MarkerControls.subscription model.markerOptions MarkerMessage
            ]


toolsAccordion : Model -> List (AccordionEntry Model Msg)
toolsAccordion (Model model) =
    [ -- For V2 we see if a single collection works...
      { label = DisplayOptions.toolLabel
      , state = Contracted
      , content = \(Model m) -> DisplayOptions.viewDisplayOptions m.displayOptions DisplayOptionsMessage
      , info = DisplayOptions.info
      , video = Just "https://youtu.be/N7zGRJvke_M"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = LoopedTrack.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                LoopedTrack.viewLoopTools
                    m.displayOptions.imperialMeasure
                    m.observations.loopiness
                    m.track
                    LoopMsg
      , info = LoopedTrack.info
      , video = Just "https://youtu.be/B3SGh8KhDu0"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = BendSmoother.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                BendSmoother.viewBendFixerPane
                    m.displayOptions.imperialMeasure
                    m.bendOptions
                    m.track
                    BendSmoothMessage
      , info = BendSmoother.info
      , video = Just "https://youtu.be/VO5jsOZmTIg"
      , isFavourite = False
      , preview3D = Just (BendSmoother.getPreview3D model.bendOptions)
      , previewProfile = Just (BendSmoother.getPreviewProfile model.displayOptions model.bendOptions)
      , previewMap = Just (BendSmoother.getPreviewMap model.displayOptions model.bendOptions)
      }
    , { label = CurveFormer.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (CurveFormer.view
                        m.displayOptions.imperialMeasure
                        m.curveFormer
                        CurveFormerMsg
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = CurveFormer.info
      , video = Just "https://youtu.be/DjdwAFkgw2o"
      , isFavourite = False
      , preview3D = Just (CurveFormer.getPreview3D model.curveFormer)
      , previewProfile = Just (CurveFormer.getPreviewProfile model.displayOptions model.curveFormer)
      , previewMap = Just (CurveFormer.getPreviewMap model.displayOptions model.curveFormer)
      }
    , { label = GradientLimiter.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (GradientLimiter.viewGradientLimitPane
                        m.gradientLimiter
                        GradientLimiter
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = GradientLimiter.info
      , video = Just "https://youtu.be/LtcYi4fzImE"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = GradientSmoother.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (GradientSmoother.viewGradientFixerPane
                        m.gradientOptions
                        GradientMessage
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = GradientSmoother.info
      , video = Just "https://youtu.be/YTY2CSl0wo8"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = Nudge.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                viewNudgeTools
                    m.displayOptions.imperialMeasure
                    m.nudgeSettings
                    NudgeMessage
      , info = Nudge.info
      , video = Just "https://youtu.be/HsH7R9SGaSs"
      , isFavourite = False
      , preview3D = Just (Nudge.getPreview3D model.nudgeSettings)
      , previewProfile = Just (Nudge.getPreviewProfile model.displayOptions model.nudgeSettings)
      , previewMap = Just (Nudge.getPreviewMap model.displayOptions model.nudgeSettings)
      }
    , { label = MoveAndStretch.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (MoveAndStretch.view
                        m.displayOptions.imperialMeasure
                        m.moveAndStretch
                        TwoWayDragMsg
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = MoveAndStretch.info
      , video = Just "https://youtu.be/9ag2iSS4OE8"
      , isFavourite = False
      , preview3D = Just (MoveAndStretch.getPreview3D model.moveAndStretch)
      , previewProfile = Just (MoveAndStretch.getPreviewProfile model.displayOptions model.moveAndStretch)
      , previewMap = Just (MoveAndStretch.getPreviewMap model.displayOptions model.moveAndStretch)
      }
    , { label = Straightener.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (Straightener.viewStraightenTools
                        m.straightenOptions
                        StraightenMessage
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = Straightener.info
      , video = Just "https://youtu.be/MQ67mzShvxg"
      , isFavourite = False
      , preview3D = Just (Straightener.getPreview3D model.straightenOptions)
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = Interpolate.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                case m.track of
                    Just _ ->
                        Interpolate.viewTools
                            m.displayOptions.imperialMeasure
                            m.insertOptions
                            InsertMessage

                    Nothing ->
                        none
      , info = Interpolate.info
      , video = Just "https://youtu.be/C3chnX2Ij_8"
      , isFavourite = False
      , preview3D = Just (Interpolate.getPreview3D model.insertOptions)
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = DeletePoints.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                viewDeleteTools m.displayOptions.imperialMeasure m.track DeleteMessage
      , info = DeletePoints.info
      , video = Nothing
      , isFavourite = False
      , preview3D = Just DeletePoints.getPreview3D
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = Flythrough.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Flythrough.flythroughControls
                    m.displayOptions.imperialMeasure
                    m.flythrough
                    FlythroughMessage
      , info = Flythrough.info
      , video = Just "https://youtu.be/lRukK-do_dE"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = Filters.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (Filters.viewFilterControls m.filterOptions
                        FilterMessage
                    )
                    model.track
                    |> Maybe.withDefault none
      , info = Filters.info
      , video = Just "https://youtu.be/N48cDi_N_x0"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }

    --, { label = Graph.toolLabel
    --  , state = Contracted
    --  , content =
    --        model.track
    --            |> Maybe.map .graph
    --            |> Maybe.andThen
    --                (Just << viewGraphControls GraphMessage)
    --            |> Maybe.withDefault none
    --  , info = Graph.info
    --  , video = Just "https://youtu.be/KSuR8PcAZYc"
    --  , isFavourite = False
    --  }
    --, { label = TrackObservations.toolLabel
    --  , state = Contracted
    --  , content =
    --        TrackObservations.overviewSummary
    --            model.displayOptions.imperialMeasure
    --            model.observations
    --  , info = "Data about the route."
    --  , video = Just "https://youtu.be/w5rfsmTF08o"
    --  , isFavourite = False
    --  }
    , { label = "Road segment"
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map (summaryData m.displayOptions.imperialMeasure) m.track
                    |> Maybe.withDefault none
      , info = "Data about the road at the orange marker."
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = "Steep climbs"
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (TrackObservations.viewSteepClimbs
                        m.problemOptions
                        ProblemMessage
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = TrackObservations.info
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content =
            \(Model m) ->
                TrackObservations.viewGradientChanges
                    m.displayOptions.imperialMeasure
                    m.problemOptions
                    m.observations
                    ProblemMessage
      , info = TrackObservations.info
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }
    , { label = "Bend problems"
      , state = Contracted
      , content =
            \(Model m) ->
                TrackObservations.viewBearingChanges
                    m.displayOptions.imperialMeasure
                    m.problemOptions
                    m.observations
                    ProblemMessage
      , info = TrackObservations.info
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      , preview3D = Nothing
      , previewProfile = Nothing
      , previewMap = Nothing
      }

    --, { label = "Intersections"
    --  , state = Contracted
    --  , content =
    --        TrackObservations.viewIntersections
    --            model.displayOptions.imperialMeasure
    --            model.problemOptions
    --            model.observations
    --            ProblemMessage
    --  , info = TrackObservations.info
    --  , video = Just "https://youtu.be/w5rfsmTF08o"
    --  , isFavourite = False
    --  }
    --, { label = StravaTools.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (StravaTools.viewStravaTab model.stravaOptions StravaMessage)
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = StravaTools.info
    --  , video = Just "https://youtu.be/31qVuc3klUE"
    --  , isFavourite = False
    --  }
    , { label = RotateRoute.toolLabel
      , state = Contracted
      , content =
            \(Model m) ->
                Maybe.map
                    (RotateRoute.view
                        m.displayOptions.imperialMeasure
                        m.rotateOptions
                        m.lastMapClick
                        RotateMessage
                    )
                    m.track
                    |> Maybe.withDefault none
      , info = RotateRoute.info
      , video = Just "https://youtu.be/P602MjJLrZ0"
      , isFavourite = False
      , preview3D = Just <| RotateRoute.getPreview3D model.rotateOptions
      , previewProfile = Nothing
      , previewMap = Just <| RotateRoute.getPreviewMap model.rotateOptions
      }

    --, { label = TrackSplitter.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (TrackSplitter.view
    --                model.displayOptions.imperialMeasure
    --                model.splitterOptions
    --                model.observations
    --                SplitterMessage
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = TrackSplitter.info
    --  , video = Nothing
    --  , isFavourite = False
    --  }
    ]



-- Reluctantly putting this here.


addToUndoStack :
    UndoEntry
    -> ModelRecord
    -> ModelRecord
addToUndoStack entry model =
    { model
        | undoStack = entry :: List.take 19 model.undoStack
        , redoStack = []
        , changeCounter = model.changeCounter + 1
    }


undo : ModelRecord -> ModelRecord
undo model =
    case ( model.track, model.undoStack ) of
        ( Just track, entry :: undos ) ->
            let
                results =
                    entry.undoFunction track

                ( prefix, changed, suffix ) =
                    ( results.before, results.edited, results.after )

                points =
                    (prefix ++ changed ++ suffix) |> prepareTrackPoints

                oldTrack =
                    { track
                        | trackPoints = points
                        , currentNode =
                            List.Extra.getAt entry.oldOrange points
                                |> Maybe.withDefault
                                    (List.head points
                                        |> Maybe.withDefault track.currentNode
                                    )
                        , markedNode =
                            case entry.oldPurple of
                                Just purple ->
                                    List.Extra.getAt purple points

                                Nothing ->
                                    Nothing
                        , earthReferenceCoordinates = results.earthReferenceCoordinates
                    }
            in
            { model
                | undoStack = undos
                , redoStack = entry :: model.redoStack
                , track = Just oldTrack
                , changeCounter = model.changeCounter - 1
            }

        _ ->
            model


redo : ModelRecord -> ModelRecord
redo model =
    case ( model.track, model.redoStack ) of
        ( Just track, entry :: redos ) ->
            let
                results =
                    entry.editFunction track

                ( prefix, middle, suffix ) =
                    ( results.before, results.edited, results.after )

                points =
                    (prefix ++ middle ++ suffix) |> prepareTrackPoints

                newTrack =
                    { track | trackPoints = points }
            in
            { model
                | redoStack = redos
                , undoStack = entry :: model.undoStack
                , track = Just newTrack
                , changeCounter = model.changeCounter + 1
            }

        _ ->
            model


undoRedoButtons model =
    row
        [ centerX
        , spacing 20
        , paddingXY 20 0
        ]
        [ button
            (width fill :: prettyButtonStyles)
            { onPress =
                case model.undoStack of
                    [] ->
                        Nothing

                    _ ->
                        Just Undo
            , label =
                case model.undoStack of
                    u :: _ ->
                        E.paragraph [ width fill ] [ E.text <| "Undo " ++ u.label ]

                    _ ->
                        E.paragraph [ width fill ] [ E.text "Nothing to undo" ]
            }
        , button
            (width fill :: prettyButtonStyles)
            { onPress =
                case model.redoStack of
                    [] ->
                        Nothing

                    _ ->
                        Just Redo
            , label =
                case model.redoStack of
                    u :: _ ->
                        E.paragraph [ width fill ] [ E.text <| "Redo " ++ u.label ]

                    _ ->
                        E.paragraph [ width fill ] [ E.text "Nothing to redo" ]
            }
        ]



-- Bunch of small stuff here I can't be bothered to make modules for.


viewAndEditFilename : ModelRecord -> Element Msg
viewAndEditFilename model =
    let
        filename =
            Maybe.withDefault "" model.filename

        trackName =
            Maybe.map .trackName model.track
                |> Maybe.join
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


saveButtonIfChanged : ModelRecord -> Element Msg
saveButtonIfChanged model =
    -- Logic might be better with two buttons, as 1CQF could be applied at any time.
    row []
        [ case ( model.track, model.changeCounter > 0 ) of
            ( Just _, True ) ->
                button
                    prettyButtonStyles
                    { onPress = Just OutputGPX
                    , label = E.text "Save as GPX file to your computer"
                    }

            _ ->
                none
        ]


outputGPX : ModelRecord -> Cmd Msg
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


processPortMessage : ModelRecord -> Encode.Value -> ( Model, Cmd Msg )
processPortMessage model json =
    -- So we don't need to keep going to the PortController.
    -- These will be Model-domain messages.
    let
        jsonMsg =
            D.decodeValue msgDecoder json

        ( lat, lon ) =
            ( D.decodeValue (D.field "lat" D.float) json
            , D.decodeValue (D.field "lon" D.float) json
            )

        elevations =
            D.decodeValue (D.field "elevations" (D.list D.float)) json

        longitudes =
            D.decodeValue (D.field "longitudes" (D.list D.float)) json

        latitudes =
            D.decodeValue (D.field "latitudes" (D.list D.float)) json
    in
    case ( jsonMsg, model.track ) of
        ( Ok "click", Just track ) ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case ( model.mapClickDebounce, lat, lon ) of
                ( False, Ok lat1, Ok lon1 ) ->
                    case searchTrackPointFromLonLat ( lon1, lat1 ) track of
                        Just point ->
                            let
                                ( outcome, cmds ) =
                                    processPostUpdateAction
                                        { model
                                            | lastMapClick = ( lon1, lat1 )
                                            , mapClickDebounce = True
                                        }
                                        (PostUpdateActions.ActionFocusMove point)
                            in
                            ( outcome
                            , Cmd.batch [ cmds, after 100 ClearMapClickDebounce ]
                            )

                        Nothing ->
                            ( Model
                                { model
                                    | lastMapClick = ( lon1, lat1 )
                                    , mapClickDebounce = True
                                }
                            , after 100 ClearMapClickDebounce
                            )

                _ ->
                    ( Model model, Cmd.none )

        --( Ok "drag", Just track ) ->
        --    case draggedOnMap json track of
        --        Just newTrack ->
        --            processPostUpdateAction
        --                model
        --                (PostUpdateActions.ActionTrackChanged
        --                    EditPreservesIndex
        --                    newTrack
        --                    "Dragged on map"
        --                )
        --Nothing ->
        --    ( model, Cmd.none )
        ( Ok "elevations", Just track ) ->
            case elevations of
                Ok mapElevations ->
                    processPostUpdateAction model
                        (PostUpdateActions.ActionTrackChanged
                            EditPreservesIndex
                            (RotateRoute.buildMapElevations mapElevations track)
                        )

                _ ->
                    ( Model model, Cmd.none )

        ( Ok "sketch", _ ) ->
            case ( longitudes, latitudes, elevations ) of
                ( Ok mapLongitudes, Ok mapLatitudes, Ok mapElevations ) ->
                    let
                        newTrack =
                            List.map3
                                (\x y z -> ( x, y, z ))
                                mapLongitudes
                                mapLatitudes
                                mapElevations
                                |> Track.trackFromMap
                    in
                    case newTrack of
                        Just track ->
                            applyTrack (Model model) track

                        Nothing ->
                            ( Model model
                            , Cmd.none
                            )

                _ ->
                    ( Model model, Cmd.none )

        ( Ok "no node", _ ) ->
            ( Model model
            , Cmd.none
            )

        ( Ok "storage.got", _ ) ->
            --TODO: Please factor this out of here.
            let
                key =
                    D.decodeValue (D.field "key" D.string) json

                value =
                    D.decodeValue (D.field "value" D.value) json
            in
            case ( key, value ) of
                ( Ok "accordion", Ok saved ) ->
                    let
                        ( restoreAccordionState, restoreAccordion ) =
                            Accordion.recoverStoredState
                                saved
                                model.toolsAccordion
                    in
                    ( Model
                        { model
                            | accordionState = restoreAccordionState
                            , toolsAccordion = restoreAccordion
                        }
                    , Cmd.none
                    )

                ( Ok "splitter", Ok splitter ) ->
                    let
                        p =
                            D.decodeValue D.int splitter
                    in
                    case p of
                        Ok pixels ->
                            ( Model
                                { model
                                    | splitInPixels = pixels
                                    , viewPanes = ViewPane.mapOverPanes (setViewPaneSize pixels) model.viewPanes
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( Model model, Cmd.none )

                ( Ok "panes", Ok saved ) ->
                    let
                        newPanes =
                            ViewPane.restorePaneState saved model.viewPanes

                        newModel =
                            { model
                                | viewPanes =
                                    ViewPane.mapOverPanes
                                        (setViewPaneSize model.splitInPixels)
                                        newPanes
                            }
                    in
                    processPostUpdateAction newModel ActionRerender

                ( Ok "display", Ok saved ) ->
                    ( Model { model | displayOptions = DisplayOptions.decodeOptions saved }
                    , Cmd.none
                    )

                _ ->
                    ( Model model, Cmd.none )

        ( Ok "storage.keys", _ ) ->
            ( Model model
            , Cmd.none
            )

        _ ->
            ( Model model, Cmd.none )
