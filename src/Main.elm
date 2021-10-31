module Main exposing (main)

import Accordion exposing (AccordionEntry, AccordionState(..), Model, view)
import BendSmoother exposing (SmoothedBend, lookForSmoothBendOption, tryBendSmoother)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
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
import GradientLimiter
import GradientSmoother
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Pointer as Pointer
import Http
import Interpolate
import Json.Decode as D
import Json.Encode as Encode
import List.Extra
import LoopedTrack
import MarkerControls exposing (markerButton, viewTrackControls)
import Maybe.Extra as Maybe
import MyIP
import Nudge exposing (NudgeEffects(..), NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import OAuth.GpxSource exposing (GpxSource(..))
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import OneClickQuickFix exposing (oneClickQuickFix)
import PortController exposing (..)
import PostUpdateActions exposing (PostUpdateAction(..))
import RotateRoute
import Scene exposing (Scene)
import SceneBuilder exposing (RenderingContext, defaultRenderingContext)
import SceneBuilderProfile
import Straightener
import StravaAuth exposing (getStravaToken, stravaButton)
import StravaTools exposing (stravaRouteOption)
import SvgPathExtractor
import Task
import Time
import Track exposing (Track, searchTrackPointFromLonLat, summaryData, updateTrackPointLonLat)
import TrackEditType exposing (TrackEditType(..))
import TrackObservations exposing (TrackObservations, deriveProblems)
import TrackPoint exposing (TrackPoint, applyGhanianTransform, prepareTrackPoints)
import TrackSplitter
import TwoWayDragControl
import Url exposing (Url)
import Utils exposing (useIcon)
import ViewPane as ViewPane exposing (ViewPane, ViewPaneAction(..), ViewPaneMessage, refreshSceneSearcher, setViewPaneSize, updatePointerInLinkedPanes)
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
    | StravaMessage StravaTools.Msg
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | RotateMessage RotateRoute.Msg
    | OneClickQuickFix
    | SvgMessage SvgPathExtractor.Msg
    | EnableMapSketchMode
    | ResizeViews Int
    | StoreSplitterPosition
    | TwoWayDragMsg TwoWayDragControl.Msg


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
    , stravaSegmentPreview : Scene
    , moveAndStretchPreview : Scene
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
    , insertOptions : Interpolate.Options
    , stravaOptions : StravaTools.Options
    , stravaAuthentication : O.Model
    , ipInfo : Maybe IpInfo
    , highlightedGraphEdge : Scene
    , gradientLimiter : GradientLimiter.Options
    , rotateOptions : RotateRoute.Options
    , lastMapClick : ( Float, Float )
    , splitterOptions : TrackSplitter.Options
    , svgData : SvgPathExtractor.Options
    , mapElevations : List Float
    , mapSketchMode : Bool
    , accordionState : Accordion.Model
    , splitInPixels : Int
    , markerOptions : MarkerControls.Options
    , twoWayDrag : TwoWayDragControl.Model
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
      , stravaSegmentPreview = []
      , moveAndStretchPreview = []
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
      , insertOptions = Interpolate.defaultOptions
      , stravaOptions = StravaTools.defaultOptions
      , stravaAuthentication = authData
      , ipInfo = Nothing
      , highlightedGraphEdge = []
      , gradientLimiter = GradientLimiter.defaultOptions
      , rotateOptions = RotateRoute.defaultOptions
      , lastMapClick = ( 0.0, 0.0 )
      , splitterOptions = TrackSplitter.defaultOptions
      , svgData = SvgPathExtractor.empty
      , mapElevations = []
      , mapSketchMode = False
      , accordionState = Accordion.defaultState
      , splitInPixels = 800
      , markerOptions = MarkerControls.defaultOptions
      , twoWayDrag = TwoWayDragControl.defaultModel
      }
        |> -- TODO: Fix Fugly Fudge. Here to make sure function is applied to model state.
           (\m -> { m | toolsAccordion = toolsAccordion m })
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , PortController.storageGetItem "accordion"
        , PortController.storageGetItem "display"
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
            ( { model | ipInfo = ipInfo }
            , Cmd.batch
                [ MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                , PortController.createMap mapInfoWithLocation
                ]
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
            let
                ( newState, newAccordion ) =
                    Accordion.update accordionMsg model.accordionState model.toolsAccordion

                newModel =
                    { model
                        | toolsAccordion = newAccordion
                        , accordionState = newState
                    }

                -- Cheeky hack to add storage message in here.
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
            let
                ( newSetttings, action ) =
                    Maybe.map (Nudge.update nudgeMsg model.nudgeSettings) model.track
                        |> Maybe.withDefault ( model.nudgeSettings, ActionNoOp )
            in
            processPostUpdateAction
                { model | nudgeSettings = newSetttings }
                action

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

        PortMessage json ->
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
                    case ( lat, lon ) of
                        ( Ok lat1, Ok lon1 ) ->
                            case searchTrackPointFromLonLat ( lon1, lat1 ) track of
                                Just point ->
                                    processPostUpdateAction
                                        { model | lastMapClick = ( lon1, lat1 ) }
                                        (PostUpdateActions.ActionFocusMove point)

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                ( Ok "drag", Just track ) ->
                    case draggedOnMap json track of
                        Just newTrack ->
                            processPostUpdateAction
                                model
                                (PostUpdateActions.ActionTrackChanged
                                    EditPreservesIndex
                                    newTrack
                                    "Dragged on map"
                                )

                        Nothing ->
                            ( model, Cmd.none )

                ( Ok "elevations", Just track ) ->
                    case elevations of
                        Ok mapElevations ->
                            let
                                newTrack =
                                    RotateRoute.applyMapElevations mapElevations track

                                newModel =
                                    -- TODO: Avoid clunk!
                                    model
                                        |> addToUndoStack "Use map elevations"
                                        |> (\m ->
                                                { m
                                                    | track = Just newTrack
                                                    , observations = deriveProblems newTrack m.problemOptions
                                                }
                                           )
                                        |> repeatTrackDerivations
                                        |> renderTrackSceneElements
                            in
                            ( newModel
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

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
                                    applyTrack model track

                                Nothing ->
                                    ( model
                                    , Cmd.none
                                    )

                        _ ->
                            ( model, Cmd.none )

                ( Ok "no node", _ ) ->
                    ( model
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
                            ( { model
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
                                    ( { model
                                        | splitInPixels = pixels
                                        , viewPanes = ViewPane.mapOverPanes (setViewPaneSize pixels) model.viewPanes
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        ( Ok "panes", Ok saved ) ->
                            let
                                newPanes =
                                    ViewPane.restorePaneState saved model.viewPanes
                            in
                            ( { model
                                | viewPanes =
                                    ViewPane.mapOverPanes
                                        (setViewPaneSize model.splitInPixels)
                                        newPanes
                              }
                            , Cmd.none
                            )

                        ( Ok "display", Ok saved ) ->
                            ( { model | displayOptions = DisplayOptions.decodeOptions saved }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                ( Ok "storage.keys", _ ) ->
                    ( model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DisplayOptionsMessage dispMsg ->
            let
                newOptions =
                    DisplayOptions.update
                        model.displayOptions
                        dispMsg
                        DisplayOptionsMessage
            in
            ( { model
                | displayOptions = newOptions
              }
                |> renderTrackSceneElements
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

        SplitterMessage splitter ->
            let
                ( newOptions, action ) =
                    TrackSplitter.update
                        splitter
                        model.splitterOptions
                        model.observations
                        model.track
                        SplitterMessage

                newModel =
                    { model
                        | splitterOptions = newOptions
                    }
            in
            processPostUpdateAction newModel action

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
            ( { model | filename = Just txt }
            , Cmd.none
            )

        OutputGPX ->
            ( { model | changeCounter = 0 }
            , outputGPX model
            )

        OneClickQuickFix ->
            case model.track of
                Just track ->
                    let
                        newTrack =
                            oneClickQuickFix track

                        newModel =
                            model
                                |> addToUndoStack "One-click Quick-fix"
                                |> (\m ->
                                        { m
                                            | track = Just newTrack
                                            , observations = deriveProblems newTrack m.problemOptions
                                        }
                                   )
                                |> repeatTrackDerivations
                                |> renderTrackSceneElements
                    in
                    ( newModel
                    , Cmd.batch <|
                        outputGPX newModel
                            :: ViewPane.makeMapCommands newTrack model.viewPanes
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                    isTrack |> applyTrack model

                Nothing ->
                    ( { model | svgData = newData }
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
                    ( { model | mapSketchMode = True }
                    , Cmd.batch
                        [ PortController.prepareSketchMap ( lon, lat )
                        , Delay.after 500 RepaintMap
                        ]
                    )

                True ->
                    ( { model | mapSketchMode = False }
                    , PortController.exitSketchMode
                    )

        ResizeViews newPosition ->
            ( { model
                | splitInPixels = newPosition
                , viewPanes = ViewPane.mapOverPanes (setViewPaneSize newPosition) model.viewPanes
              }
            , Cmd.batch
                [ Delay.after 50 RepaintMap
                , PortController.storageSetItem "splitter" (Encode.int model.splitInPixels)
                ]
            )

        StoreSplitterPosition ->
            ( model
            , PortController.storageSetItem "splitter" (Encode.int model.splitInPixels)
            )

        TwoWayDragMsg dragMsg ->
            let
                ( newOptions, action ) =
                    Maybe.map
                        (TwoWayDragControl.update
                            dragMsg
                            model.twoWayDrag
                            TwoWayDragMsg
                        )
                        model.track
                        |> Maybe.withDefault ( model.twoWayDrag, ActionNoOp )
            in
            processPostUpdateAction
                { model | twoWayDrag = newOptions }
                action


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


processPostUpdateAction : Model -> PostUpdateAction (Cmd Msg) -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    -- I doubt that will ever be true.
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType newTrack undoMsg ) ->
            ( model
                |> addToUndoStack undoMsg
                |> reflectNewTrackViaGraph newTrack editType
            , Cmd.batch <| ViewPane.makeMapCommands newTrack model.viewPanes
            )

        ( Just track, ActionRerender ) ->
            -- Use this after Undo/Redo to avoid pushing change onto stack.
            ( model
                |> reflectNewTrackViaGraph track EditNoOp
            , Cmd.batch <| ViewPane.makeMapCommands track model.viewPanes
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
            ( newModel
                |> reflectNewTrackViaGraph track EditNoOp
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
                [ PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.twoWayDrag ]
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
                [ PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.twoWayDrag
                , if ViewPane.mapPaneIsLinked model.viewPanes then
                    PortController.centreMapOnCurrent updatedTrack

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
                [ PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.twoWayDrag ]
            )

        ( Just track, ActionRepaintMap ) ->
            ( model
            , Delay.after 50 RepaintMap
            )

        ( Just track, ActionToggleMapDragging isDragging ) ->
            ( model
            , PortController.toggleDragging isDragging track
            )

        ( Just track, ActionFetchMapElevations ) ->
            ( model
            , PortController.requestElevations
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
                [ PortController.addMarkersToMap track bendPreview nudgePreview model.twoWayDrag ]
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
    case Track.trackFromGpx content of
        Just track ->
            applyTrack model track

        Nothing ->
            ( model, Cmd.none )


applyTrack : Model -> Track -> ( Model, Cmd Msg )
applyTrack model track =
    let
        ( newViewPanes, mapCommands ) =
            ( List.map (ViewPane.resetAllViews track) model.viewPanes
            , ViewPane.initialiseMap track model.viewPanes
                ++ [ PortController.storageGetItem "panes"
                   , PortController.storageGetItem "splitter"
                   , Delay.after 500 RepaintMap
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
      }
        |> repeatTrackDerivations
    , Cmd.batch mapCommands
    )


processViewPaneMessage : ViewPaneMessage -> Model -> Track -> ( Model, Cmd Msg )
processViewPaneMessage innerMsg model track =
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
            in
            ( { updatedModel
                | viewPanes = updatedPanes
              }
            , Cmd.batch
                [ PortController.storageSetItem "panes" (ViewPane.storePaneLayout updatedPanes)
                , Delay.after 50 RepaintMap
                ]
            )

        ViewPane.PaneNoOp ->
            ( updatedModel, Cmd.none )


processGraphMessage : Graph.Msg -> Model -> Track -> ( Model, PostUpdateActions.PostUpdateAction msg )
processGraphMessage innerMsg model isTrack =
    let
        ( newGraph, action ) =
            Graph.update innerMsg
                isTrack.trackPoints
                isTrack.currentNode
                isTrack.markedNode
                isTrack.graph

        newTrack =
            { isTrack | graph = newGraph }

        modelWithUpdatedGraph =
            { model | track = Just newTrack }
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
            let
                trackFromGraph =
                    { isTrack
                        | trackPoints =
                            Maybe.map Graph.publishUserRoute newGraph
                                |> Maybe.withDefault []
                        , graph = Nothing
                    }

                modelFromGraph =
                    { model | track = Just trackFromGraph }
            in
            ( model
            , PostUpdateActions.ActionTrackChanged
                EditNoOp
                trackFromGraph
                "Leave Graph mode"
            )

        GraphShowTraversal ->
            ( modelWithUpdatedGraph
            , PostUpdateActions.ActionPreview
            )


reflectNewTrackViaGraph : Track -> TrackEditType -> Model -> Model
reflectNewTrackViaGraph newTrack editType model =
    -- We need this in case we have a Graph in effect, and the edits need to be
    -- reflected in the graph and the final new route dervied from the graph again.
    -- If there's no graph, it's basically a noop.
    case model.track of
        Just oldTrack ->
            let
                orange =
                    oldTrack.currentNode

                purple =
                    -- Need to get edit region to help the graph assess the changes.
                    Maybe.withDefault oldTrack.currentNode oldTrack.markedNode

                editRegion =
                    ( min orange.index purple.index
                    , max orange.index purple.index
                    )

                changeInLength =
                    List.length newTrack.trackPoints - List.length oldTrack.trackPoints

                newOrange =
                    Maybe.withDefault orange <|
                        if orange.index <= purple.index then
                            List.Extra.getAt orange.index newTrack.trackPoints

                        else
                            List.Extra.getAt (orange.index + changeInLength) newTrack.trackPoints

                newPurple =
                    case oldTrack.markedNode of
                        Just mark ->
                            if mark.index <= orange.index then
                                List.Extra.getAt mark.index newTrack.trackPoints

                            else
                                List.Extra.getAt (mark.index + changeInLength) newTrack.trackPoints

                        Nothing ->
                            Nothing

                newGraph =
                    Graph.updateWithNewTrack
                        newTrack.graph
                        oldTrack.trackPoints
                        -- Pre-edit baseline points.
                        editRegion
                        -- Where the markers were.
                        newTrack.trackPoints
                        -- Post-edit track points.
                        editType

                newPointsFromGraph =
                    Maybe.map Graph.walkTheRoute newGraph
                        |> Maybe.withDefault newTrack.trackPoints

                trackWithNewRoute =
                    case newTrack.graph of
                        Just graph ->
                            { newTrack
                                | trackPoints = newPointsFromGraph
                                , currentNode = newOrange
                                , markedNode = newPurple
                                , graph = newGraph
                            }

                        Nothing ->
                            newTrack
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

                newTrack =
                    { isTrack
                        | trackPoints = earthTrack
                        , currentNode = newOrange
                        , markedNode = newPurple
                    }
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
                ++ model.moveAndStretchPreview
                ++ model.nudgePreview
                ++ model.bendPreview
                ++ model.stravaSegmentPreview
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
        stretchMarker =
            Maybe.map (TwoWayDragControl.getStretchPointer model.twoWayDrag)
                model.track
                |> Maybe.join

        updatedMarkers =
            -- Kind of ugly having the stretchPointer here. Maybe v3 will fix that!
            Maybe.map (SceneBuilder.renderMarkers stretchMarker) model.track
                |> Maybe.withDefault []

        updatedProfileMarkers =
            Maybe.map (SceneBuilderProfile.renderMarkers model.displayOptions) model.track
                |> Maybe.withDefault []

        updatedMoveAndStretchSettings =
            let
                settings =
                    model.twoWayDrag
            in
            if
                Accordion.tabIsOpen "Move & Stretch" model.toolsAccordion
                    && TwoWayDragControl.settingNotZero settings
            then
                Maybe.map (TwoWayDragControl.preview model.twoWayDrag) model.track
                    |> Maybe.withDefault settings

            else
                { settings | preview = [] }

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
            if Accordion.tabIsOpen "Bend smoother classic" model.toolsAccordion then
                Maybe.map (tryBendSmoother model.bendOptions) model.track
                    |> Maybe.withDefault model.bendOptions

            else
                BendSmoother.defaultOptions

        updatedStravaOptions =
            -- TODO: ?? Move pointers to discovered paste start and end ??
            let
                options =
                    model.stravaOptions
            in
            if Accordion.tabIsOpen "Strava" model.toolsAccordion then
                { options
                    | preview =
                        Maybe.map (StravaTools.preview options) model.track
                            |> Maybe.withDefault []
                }

            else
                { options | preview = [] }

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
        , stravaSegmentPreview = SceneBuilder.previewStravaSegment updatedStravaOptions.preview
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
        , moveAndStretchPreview =
            SceneBuilder.previewMoveAndStretch
                updatedMoveAndStretchSettings.preview
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
        , stravaRouteOption
            model.stravaAuthentication
            model.stravaOptions
            StravaMessage
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


footer : Model -> Element Msg
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


mapSketchEnable : Model -> Element Msg
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


contentArea : Model -> Element Msg
contentArea model =
    let
        leftPane =
            column
                [ width fill, alignTop ]
                [ viewAllPanes
                    model.viewPanes
                    model.displayOptions
                    ( model.completeScene, model.completeProfile )
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
                        (updatedAccordion model.toolsAccordion toolsAccordion model)
                        AccordionMessage
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


viewAllPanes : List ViewPane -> DisplayOptions -> ( Scene, Scene ) -> (ViewPaneMessage -> Msg) -> Element Msg
viewAllPanes panes options ( scene, profile ) wrapper =
    wrappedRow [ width fill, spacing 10 ] <|
        List.map
            (ViewPane.view ( scene, profile ) options wrapper)
            panes


updatedAccordion :
    List (AccordionEntry Msg)
    -> (Model -> List (AccordionEntry Msg))
    -> Model
    -> List (AccordionEntry Msg)
updatedAccordion currentAccordion referenceAccordion model =
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


toolsAccordion : Model -> List (AccordionEntry Msg)
toolsAccordion model =
    [ -- For V2 we see if a single collection works...
      { label = "Visual styles"
      , state = Contracted
      , content = DisplayOptions.viewDisplayOptions model.displayOptions DisplayOptionsMessage
      , info = DisplayOptions.info
      , video = Just "https://youtu.be/N7zGRJvke_M"
      , isFavourite = False
      }
    , { label = "LoopedTrack maker"
      , state = Contracted
      , content =
            LoopedTrack.viewLoopTools
                model.displayOptions.imperialMeasure
                model.observations.loopiness
                model.track
                LoopMsg
      , info = LoopedTrack.info
      , video = Just "https://youtu.be/B3SGh8KhDu0"
      , isFavourite = False
      }
    , { label = "Bend smoother classic"
      , state = Contracted
      , content =
            BendSmoother.viewBendFixerPane
                model.displayOptions.imperialMeasure
                model.bendOptions
                BendSmoothMessage
      , info = BendSmoother.info
      , video = Just "https://youtu.be/VO5jsOZmTIg"
      , isFavourite = False
      }
    , { label = "Limit gradients"
      , state = Contracted
      , content =
            Maybe.map
                (GradientLimiter.viewGradientLimitPane
                    model.gradientLimiter
                    GradientLimiter
                )
                model.track
                |> Maybe.withDefault none
      , info = GradientLimiter.info
      , video = Just "https://youtu.be/LtcYi4fzImE"
      , isFavourite = False
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
      , video = Just "https://youtu.be/YTY2CSl0wo8"
      , isFavourite = False
      }
    , { label = "Nudge"
      , state = Contracted
      , content =
            viewNudgeTools
                model.displayOptions.imperialMeasure
                model.nudgeSettings
                NudgeMessage
      , info = Nudge.info
      , video = Just "https://youtu.be/HsH7R9SGaSs"
      , isFavourite = False
      }
    , { label = "Move & Stretch"
      , state = Contracted
      , content =
            Maybe.map
                (TwoWayDragControl.view
                    model.displayOptions.imperialMeasure
                    model.twoWayDrag
                    TwoWayDragMsg
                )
                model.track
                |> Maybe.withDefault none
      , info = TwoWayDragControl.info
      , video = Just "https://youtu.be/9ag2iSS4OE8"
      , isFavourite = False
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
      , video = Just "https://youtu.be/MQ67mzShvxg"
      , isFavourite = False
      }
    , { label = "Interpolate"
      , state = Contracted
      , content =
            case model.track of
                Just _ ->
                    Interpolate.viewTools
                        model.displayOptions.imperialMeasure
                        model.insertOptions
                        InsertMessage

                Nothing ->
                    none
      , info = Interpolate.info
      , video = Just "https://youtu.be/C3chnX2Ij_8"
      , isFavourite = False
      }
    , { label = "Delete"
      , state = Contracted
      , content = viewDeleteTools model.track DeleteMessage
      , info = DeletePoints.info
      , video = Nothing
      , isFavourite = False
      }
    , { label = "Fly-through"
      , state = Contracted
      , content =
            Flythrough.flythroughControls
                model.displayOptions.imperialMeasure
                model.flythrough
                FlythroughMessage
      , info = Flythrough.info
      , video = Just "https://youtu.be/lRukK-do_dE"
      , isFavourite = False
      }
    , { label = "Track smoothers 3D"
      , state = Contracted
      , content =
            Maybe.map
                (Filters.viewFilterControls model.filterOptions
                    FilterMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = Filters.info
      , video = Just "https://youtu.be/N48cDi_N_x0"
      , isFavourite = False
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
      , video = Just "https://youtu.be/KSuR8PcAZYc"
      , isFavourite = False
      }
    , { label = "Route summary"
      , state = Contracted
      , content =
            TrackObservations.overviewSummary
                model.displayOptions.imperialMeasure
                model.observations
      , info = "Data about the route."
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      }
    , { label = "Road segment"
      , state = Contracted
      , content =
            Maybe.map (summaryData model.displayOptions.imperialMeasure) model.track
                |> Maybe.withDefault none
      , info = "Data about the road at the orange marker."
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
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
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      }
    , { label = "Gradient problems"
      , state = Contracted
      , content =
            TrackObservations.viewGradientChanges
                model.displayOptions.imperialMeasure
                model.problemOptions
                model.observations
                ProblemMessage
      , info = TrackObservations.info
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      }
    , { label = "Bend problems"
      , state = Contracted
      , content =
            TrackObservations.viewBearingChanges
                model.displayOptions.imperialMeasure
                model.problemOptions
                model.observations
                ProblemMessage
      , info = TrackObservations.info
      , video = Just "https://youtu.be/w5rfsmTF08o"
      , isFavourite = False
      }
    , { label = "Strava"
      , state = Contracted
      , content =
            Maybe.map
                (StravaTools.viewStravaTab model.stravaOptions StravaMessage)
                model.track
                |> Maybe.withDefault none
      , info = StravaTools.info
      , video = Just "https://youtu.be/31qVuc3klUE"
      , isFavourite = False
      }
    , { label = "Lift & Shift"
      , state = Contracted
      , content =
            Maybe.map
                (RotateRoute.view
                    model.displayOptions.imperialMeasure
                    model.rotateOptions
                    model.lastMapClick
                    RotateMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = RotateRoute.info
      , video = Just "https://youtu.be/P602MjJLrZ0"
      , isFavourite = False
      }
    , { label = "Splitter & Joiner"
      , state = Contracted
      , content =
            Maybe.map
                (TrackSplitter.view
                    model.displayOptions.imperialMeasure
                    model.splitterOptions
                    model.observations
                    SplitterMessage
                )
                model.track
                |> Maybe.withDefault none
      , info = TrackSplitter.info
      , video = Nothing
      , isFavourite = False
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


viewAndEditFilename : Model -> Element Msg
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


saveButtonIfChanged : Model -> Element Msg
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
