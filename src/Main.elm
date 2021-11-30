module Main exposing (main)

--import BendSmoother exposing (SmoothedBend, tryBendSmoother)
--import CurveFormer
--import DeletePoints exposing (Action(..), viewDeleteTools)
--import Flythrough exposing (Flythrough)
--import GradientLimiter
--import GradientSmoother
--import Interpolate
--import LoopedTrack
--import RotateRoute
--import Straightener
--import StravaTools exposing (stravaRouteOption)

import Accordion exposing (AccordionEntry, AccordionState(..), Model, view)
import BoundingBox3d
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Delay exposing (after)
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
import GeoCodeDecoders exposing (IpInfo)
import GpxSource exposing (..)
import Graph exposing (Graph, GraphActionImpact(..), viewGraphControls)
import Html.Attributes exposing (id, style)
import Http
import Json.Decode as D
import Json.Encode as Encode
import Length
import List.Extra
import MarkerControls exposing (markerButton, viewTrackControls)
import Maybe.Extra as Maybe
import MoveAndStretch
import MyIP
import Nudge exposing (NudgeSettings, defaultNudgeSettings, viewNudgeTools)
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import OneClickQuickFix exposing (oneClickQuickFix)
import PortController exposing (..)
import PostUpdateActions exposing (PostUpdateAction(..), TrackEditType(..), UndoEntry)
import Quantity
import Scene exposing (Scene)
import SceneBuilder exposing (RenderingContext, defaultRenderingContext)
import SceneBuilderProfile
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
      --| DeleteMessage DeletePoints.Msg
    | ViewPaneMessage ViewPane.ViewPaneMessage
    | OAuthMessage OAuthMsg
    | PortMessage Encode.Value
    | RepaintMap
    | DisplayOptionsMessage DisplayOptions.Msg
      --| BendSmoothMessage BendSmoother.Msg
      --| LoopMsg LoopedTrack.Msg
      --| GradientMessage GradientSmoother.Msg
      --| GradientLimiter GradientLimiter.Msg
      --| StraightenMessage Straightener.Msg
      --| FlythroughMessage Flythrough.Msg
    | FilterMessage Filters.Msg
    | ProblemMessage TrackObservations.Msg
      --| InsertMessage Interpolate.Msg
    | SplitterMessage TrackSplitter.Msg
    | UserChangedFilename String
    | OutputGPX
      --| StravaMessage StravaTools.Msg
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
      --| RotateMessage RotateRoute.Msg
    | OneClickQuickFix
    | SvgMessage SvgPathExtractor.Msg
    | EnableMapSketchMode
    | ResizeViews Int
    | StoreSplitterPosition
    | TwoWayDragMsg MoveAndStretch.Msg



--| CurveFormerMsg CurveFormer.Msg


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
    , completeScene : Scene
    , completeProfile : Scene
    , renderingContext : Maybe RenderingContext
    , viewPanes : List ViewPane
    , track : Maybe Track
    , markerPositionAtLastSceneBuild : Quantity.Quantity Float Length.Meters
    , toolsAccordion : List (AccordionEntry Msg)
    , nudgeSettings : NudgeSettings
    , undoStack : List UndoEntry
    , redoStack : List UndoEntry
    , changeCounter : Int
    , displayOptions : DisplayOptions.DisplayOptions

    --, bendOptions : BendSmoother.BendOptions
    , observations : TrackObservations

    --, gradientOptions : GradientSmoother.Options
    --, straightenOptions : Straightener.Options
    --, flythrough : Flythrough.Options
    --, filterOptions : Filters.Options
    , problemOptions : TrackObservations.Options

    --, insertOptions : Interpolate.Options
    --, stravaOptions : StravaTools.Options
    , stravaAuthentication : O.Model
    , ipInfo : Maybe IpInfo

    --, gradientLimiter : GradientLimiter.Options
    --, rotateOptions : RotateRoute.Options
    , lastMapClick : ( Float, Float )

    --, splitterOptions : TrackSplitter.Options
    , svgData : SvgPathExtractor.Options
    , mapElevations : List Float
    , mapSketchMode : Bool
    , accordionState : Accordion.Model
    , splitInPixels : Int
    , markerOptions : MarkerControls.Options

    --, moveAndStretch : MoveAndStretch.Model
    --, curveFormer : CurveFormer.Model
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

      --, bendOptions = BendSmoother.defaultOptions
      , observations = TrackObservations.defaultObservations

      --, gradientOptions = GradientSmoother.defaultOptions
      --, straightenOptions = Straightener.defaultOptions
      --, flythrough = Flythrough.defaultOptions
      --, filterOptions = Filters.defaultOptions
      , problemOptions = TrackObservations.defaultOptions

      --, insertOptions = Interpolate.defaultOptions
      --, stravaOptions = StravaTools.defaultOptions
      , stravaAuthentication = authData
      , ipInfo = Nothing

      --, gradientLimiter = GradientLimiter.defaultOptions
      --, rotateOptions = RotateRoute.defaultOptions
      , lastMapClick = ( 0.0, 0.0 )

      --, splitterOptions = TrackSplitter.defaultOptions
      , svgData = SvgPathExtractor.empty
      , mapElevations = []
      , mapSketchMode = False
      , accordionState = Accordion.defaultState
      , splitInPixels = 800
      , markerOptions = MarkerControls.defaultOptions

      --, moveAndStretch = MoveAndStretch.defaultModel
      --, curveFormer = CurveFormer.defaultModel
      }
        |> -- TODO: Remove this Fugly Fudge. Here to make sure function is applied to model state.
           (\m -> { m | toolsAccordion = toolsAccordion m })
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , PortController.storageGetItem "accordion"
        , PortController.storageGetItem "display"

        --, PortController.storageGetItem "panes"
        ]
    )



--TODO: Don't restore this. It's very, you know, imperative.
--passFlythroughToContext : Maybe Flythrough -> ViewingContext -> ViewingContext
--passFlythroughToContext flight context =
--    { context | flythrough = flight }


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

        --Tick newTime ->
        --    let
        --        flythrough =
        --            model.flythrough
        --
        --        updatedFlythrough =
        --            Flythrough.advanceFlythrough
        --                newTime
        --                { flythrough | modelTime = newTime }
        --    in
        --    ( { model
        --        | time = newTime
        --        , flythrough = updatedFlythrough
        --        , viewPanes =
        --            ViewPane.mapOverAllContexts
        --                (passFlythroughToContext updatedFlythrough.flythrough)
        --                model.viewPanes
        --      }
        --    , Cmd.none
        --    )
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
                            Nudge.update nudgeMsg model.displayOptions.imperialMeasure model.nudgeSettings track
                    in
                    processPostUpdateAction
                        { model | nudgeSettings = newSetttings }
                        action

                Nothing ->
                    ( model, Cmd.none )

        --InsertMessage insertMsg ->
        --    let
        --        ( newSettings, action ) =
        --            Maybe.map
        --                (Interpolate.update
        --                    insertMsg
        --                    model.insertOptions
        --                )
        --                model.track
        --                |> Maybe.withDefault ( model.insertOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | insertOptions = newSettings }
        --        action
        --
        --DeleteMessage deleteMsg ->
        --    let
        --        action =
        --            Maybe.map (DeletePoints.update model.displayOptions.imperialMeasure deleteMsg) model.track
        --                |> Maybe.withDefault ActionNoOp
        --    in
        --    processPostUpdateAction model action
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
                --( Ok "elevations", Just track ) ->
                --    case elevations of
                --        Ok mapElevations ->
                --            let
                --                newTrack =
                --                    RotateRoute.applyMapElevations mapElevations track
                --
                --                newModel =
                --                    -- TODO: Avoid clunk!
                --                    model
                --                        |> addToUndoStack "Use map elevations"
                --                        |> (\m ->
                --                                { m
                --                                    | track = Just newTrack
                --                                    , observations = deriveProblems newTrack m.problemOptions
                --                                }
                --                           )
                --                        |> repeatTrackDerivations
                --                        |> renderTrackSceneElements
                --            in
                --            ( newModel
                --            , Cmd.none
                --            )
                --_ ->
                --( model, Cmd.none )
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
                |> composeScene
            , PortController.storageSetItem "display" (DisplayOptions.encodeOptions newOptions)
            )

        --BendSmoothMessage bendMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (BendSmoother.update bendMsg model.bendOptions) model.track
        --                |> Maybe.withDefault ( model.bendOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | bendOptions = newOptions }
        --        action
        --
        --LoopMsg loopMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (LoopedTrack.update loopMsg model.observations.loopiness) model.track
        --                |> Maybe.withDefault ( model.observations.loopiness, ActionNoOp )
        --
        --        oldObs =
        --            model.observations
        --
        --        newObs =
        --            { oldObs | loopiness = newOptions }
        --    in
        --    processPostUpdateAction
        --        { model | observations = newObs }
        --        action
        --
        --GradientLimiter limitMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (GradientLimiter.update limitMsg model.gradientLimiter)
        --                model.track
        --                |> Maybe.withDefault ( model.gradientLimiter, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | gradientLimiter = newOptions }
        --        action
        --
        --GradientMessage gradMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (GradientSmoother.update gradMsg model.gradientOptions)
        --                model.track
        --                |> Maybe.withDefault ( model.gradientOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | gradientOptions = newOptions }
        --        action
        --
        --StraightenMessage straight ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (Straightener.update straight model.straightenOptions)
        --                model.track
        --                |> Maybe.withDefault ( model.straightenOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | straightenOptions = newOptions }
        --        action
        --
        --FlythroughMessage flythroughMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map
        --                (Flythrough.update
        --                    model.flythrough
        --                    flythroughMsg
        --                    FlythroughMessage
        --                )
        --                model.track
        --                |> Maybe.withDefault ( model.flythrough, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model
        --            | flythrough = newOptions
        --            , viewPanes =
        --                ViewPane.mapOverAllContexts
        --                    (passFlythroughToContext newOptions.flythrough)
        --                    model.viewPanes
        --        }
        --        action
        --
        --FilterMessage filter ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (Filters.update filter model.filterOptions model.observations)
        --                model.track
        --                |> Maybe.withDefault ( model.filterOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | filterOptions = newOptions }
        --        action
        --
        --RotateMessage rotate ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map (RotateRoute.update rotate model.rotateOptions model.lastMapClick)
        --                model.track
        --                |> Maybe.withDefault ( model.rotateOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | rotateOptions = newOptions }
        --        action
        --
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
        --ProblemMessage probMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map
        --                (TrackObservations.update
        --                    probMsg
        --                    model.problemOptions
        --                    model.bendOptions.segments
        --                )
        --                model.track
        --                |> Maybe.withDefault ( model.problemOptions, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | problemOptions = newOptions }
        --        action
        UserChangedFilename txt ->
            ( { model | filename = Just txt }
            , Cmd.none
            )

        OutputGPX ->
            ( { model | changeCounter = 0 }
            , outputGPX model
            )

        --OneClickQuickFix ->
        --    case model.track of
        --        Just track ->
        --            let
        --                newTrack =
        --                    oneClickQuickFix track
        --
        --                newModel =
        --                    model
        --                        |> addToUndoStack "One-click Quick-fix"
        --                        |> (\m ->
        --                                { m
        --                                    | track = Just newTrack
        --                                    , observations = deriveProblems newTrack m.problemOptions
        --                                }
        --                           )
        --                        |> repeatTrackDerivations
        --                        |> renderTrackSceneElements
        --            in
        --            ( newModel
        --            , Cmd.batch
        --                [ outputGPX newModel
        --                , ViewPane.makeMapCommands newTrack model.viewPanes
        --                ]
        --            )
        --
        --        Nothing ->
        --            ( model, Cmd.none )
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

        --TwoWayDragMsg dragMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map
        --                (MoveAndStretch.update
        --                    dragMsg
        --                    model.moveAndStretch
        --                    TwoWayDragMsg
        --                )
        --                model.track
        --                |> Maybe.withDefault ( model.moveAndStretch, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | moveAndStretch = newOptions }
        --        action
        --CurveFormerMsg curveMsg ->
        --    let
        --        ( newOptions, action ) =
        --            Maybe.map
        --                (CurveFormer.update
        --                    curveMsg
        --                    model.curveFormer
        --                    CurveFormerMsg
        --                )
        --                model.track
        --                |> Maybe.withDefault ( model.curveFormer, ActionNoOp )
        --    in
        --    processPostUpdateAction
        --        { model | curveFormer = newOptions }
        --        action
        _ ->
            ( model, Cmd.none )


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



--
--updatePreviews : Model -> Track -> ( Track, Track )
--updatePreviews model track =
--    -- Interim step in re-factor!
--    let
--        bendPreview =
---- Give classic smoother priority but if not used see if the Curve Former has anything.
--if Accordion.tabIsOpen BendSmoother.toolLabel model.toolsAccordion then
--    { track
--        | trackPoints =
--            Maybe.map .nodes model.bendOptions.smoothedBend
--                |> Maybe.withDefault []
--    }
--
--else if Accordion.tabIsOpen CurveFormer.toolLabel model.toolsAccordion then
--    { track | trackPoints = model.curveFormer.newTrackPoints }
--
--else
--        { track | trackPoints = [] }
--
--    nudgePreview =
--        Nudge.getPreview model.nudgeSettings track
--in
--( bendPreview, nudgePreview )


processPostUpdateAction : Model -> PostUpdateAction Track (Cmd Msg) -> ( Model, Cmd Msg )
processPostUpdateAction model action =
    -- This should be the one place from where actions are orchestrated.
    -- I doubt that will ever be true.
    case ( model.track, action ) of
        ( Just track, ActionTrackChanged editType undoEntry ) ->
            let
                ( prefix, changed, suffix ) =
                    undoEntry.editFunction track

                newTrack =
                    { track
                        | trackPoints =
                            (prefix ++ changed ++ suffix) |> prepareTrackPoints
                    }
            in
            ( { model | track = Just newTrack }
                |> addToUndoStack undoEntry
                |> reflectNewTrackViaGraph newTrack editType
                |> repeatTrackDerivations
                |> composeScene
            , Cmd.batch
                [ ViewPane.makeMapCommands newTrack model.viewPanes
                , Delay.after 50 RepaintMap
                ]
            )

        ( Just track, ActionRerender ) ->
            -- Use this after Undo/Redo to avoid pushing change onto stack.
            ( model
                |> reflectNewTrackViaGraph track EditNoOp
                |> repeatTrackDerivations
                |> composeScene
            , Cmd.batch
                [ ViewPane.makeMapCommands track model.viewPanes
                , Delay.after 50 RepaintMap
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
            ( newModel
                |> reflectNewTrackViaGraph track EditNoOp
            , Cmd.batch
                [ ViewPane.makeMapCommands newTrack newModel.viewPanes
                , Delay.after 50 RepaintMap
                ]
            )

        ( Just track, ActionPointerMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }

                --
                --( bendPreview, nudgePreview ) =
                --    updatePreviews model updatedTrack
            in
            ( { model | track = Just updatedTrack }
                |> composeScene
            , Cmd.batch
                [--PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.moveAndStretch
                ]
            )

        ( Just track, ActionFocusMove tp ) ->
            let
                updatedTrack =
                    { track | currentNode = tp }

                --
                --( bendPreview, nudgePreview ) =
                --    updatePreviews model updatedTrack
            in
            ( { model
                | track = Just updatedTrack
                , viewPanes = ViewPane.mapOverPanes (updatePointerInLinkedPanes tp) model.viewPanes
              }
                |> composeScene
            , Cmd.batch
                [ Cmd.none

                --, PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.moveAndStretch
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

                --
                --( bendPreview, nudgePreview ) =
                --    updatePreviews model updatedTrack
            in
            ( { model | track = Just updatedTrack }
                |> composeScene
            , Cmd.batch
                [ Cmd.none

                --, PortController.addMarkersToMap updatedTrack bendPreview nudgePreview model.moveAndStretch
                ]
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
            ( model |> composeScene
            , --if isMapVisible model.viewPanes then
              --    PortController.addMarkersToMap
              --        track
              --        bendPreview
              --        nudgePreview
              --        model.moveAndStretch
              --
              --else
              Cmd.none
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
            applyTrack
                { model
                    | viewPanes =
                        -- Force third person view on first file load.
                        if model.track == Nothing then
                            ViewPane.viewPanesWithTrack

                        else
                            model.viewPanes
                }
                track

        Nothing ->
            ( model, Cmd.none )


applyTrack : Model -> Track -> ( Model, Cmd Msg )
applyTrack model track =
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
      }
        |> repeatTrackDerivations
    , mapCommands
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
            ( updatedModel, Cmd.none )


processGraphMessage : Graph.Msg -> Model -> Track -> ( Model, PostUpdateActions.PostUpdateAction trck msg )
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


reflectNewTrackViaGraph : Track -> TrackEditType -> Model -> Model
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
                        , spatialIndex = Track.buildSpatialIndex earthTrack isTrack.box
                    }
            in
            { model
                | track = Just newTrack
                , observations = deriveProblems newTrack model.problemOptions
            }

        Nothing ->
            model



{-
   TODO NEXT - derive all elements with as few list passes as possible,
   not keeping partials, using reverseCons instead of concatenation.
   Can we do it all from a single pass, is the challenge.
   Visual options should be used to compose a list of functions (think Applicative).
-}


composeScene : Model -> Model
composeScene model =
    --TODO: Reinstate variable detail rendering. Code should be in makeReducedTrack ??
    --case model.track of
    --    Just hasTrack ->
    --        -- Force full track repaint if marker moved sufficiently
    --        let
    --            ( xSize, ySize, _ ) =
    --                BoundingBox3d.dimensions hasTrack.box
    --
    --            threshold =
    --                Quantity.max xSize ySize
    --                    |> Quantity.multiplyBy (0.5 ^ (1 + model.displayOptions.levelOfDetailThreshold))
    --        in
    --        if
    --            model.displayOptions.levelOfDetailThreshold
    --                > 0.0
    --                && (Quantity.abs
    --                        (hasTrack.currentNode.distanceFromStart
    --                            |> Quantity.minus model.markerPositionAtLastSceneBuild
    --                        )
    --                        |> Quantity.greaterThan threshold
    --                   )
    --        then
    --            renderTrackSceneElements model
    --
    --        else
    --            model
    --
    --    Nothing ->
    --        model
    { model
        | completeScene =
            combineLists
                [ renderVaryingSceneElements model
                , renderTrackSceneElements model
                ]
        , completeProfile = []
    }


renderVaryingSceneElements : Model -> Scene
renderVaryingSceneElements model =
    let
        latestModel =
            model

        --stretchMarker =
        --    if Accordion.tabIsOpen MoveAndStretch.toolLabel latestModel.toolsAccordion then
        --        Maybe.map (MoveAndStretch.getStretchPointer latestModel.moveAndStretch)
        --            latestModel.track
        --            |> Maybe.join
        --
        --    else
        --        Nothing
        updatedMarkers =
            -- Kind of ugly having the stretchPointer here. Maybe v3 will fix that!
            case latestModel.track of
                Just isTrack ->
                    let
                        whiteMarker =
                            SceneBuilder.renderMarkers Nothing isTrack

                        renderingLimits =
                            if model.displayOptions.showRenderingLimit then
                                SceneBuilder.renderMRLimits isTrack

                            else
                                []
                    in
                    whiteMarker ++ renderingLimits

                Nothing ->
                    []

        updatedProfileMarkers =
            Maybe.map
                (SceneBuilderProfile.renderMarkers
                    latestModel.displayOptions
                    Nothing
                 -- stretchMarker
                )
                latestModel.track
                |> Maybe.withDefault []

        --updatedMoveAndStretchSettings =
        --    let
        --        settings =
        --            latestModel.moveAndStretch
        --    in
        --    if
        --        Accordion.tabIsOpen MoveAndStretch.toolLabel latestModel.toolsAccordion
        --            && MoveAndStretch.settingNotZero latestModel.moveAndStretch
        --    then
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
        --curveFormerWithPreview =
        --    Maybe.map (CurveFormer.preview latestModel.curveFormer) latestModel.track
        --        |> Maybe.withDefault latestModel.curveFormer
        --
        --curveFormerCircle =
        --    if Accordion.tabIsOpen CurveFormer.toolLabel latestModel.toolsAccordion then
        --        CurveFormer.getPreview curveFormerWithPreview
        --
        --    else
        --        []
    in
    updatedMarkers


renderTrackSceneElements : Model -> Scene
renderTrackSceneElements model =
    case model.track of
        Just isTrack ->
            let
                ( xSize, ySize, _ ) =
                    BoundingBox3d.dimensions isTrack.box

                threshold =
                    -- Size of box within which all trackpoints are rendered.
                    Quantity.max xSize ySize
                        |> Quantity.multiplyBy (0.5 ^ model.displayOptions.levelOfDetailThreshold)

                reducedTrack =
                    -- Render all trackpoints within specified area around marker,
                    -- with reduced detail outside.
                    if model.displayOptions.levelOfDetailThreshold == 0.0 then
                        isTrack

                    else
                        Track.makeReducedTrack isTrack threshold

                updatedScene =
                    if is3dVisible model.viewPanes then
                        SceneBuilder.renderTrack model.displayOptions reducedTrack

                    else
                        []

                updatedProfile =
                    if isProfileVisible model.viewPanes then
                        SceneBuilderProfile.renderTrack model.displayOptions reducedTrack

                    else
                        []

                updatedTerrain =
                    if is3dVisible model.viewPanes then
                        SceneBuilder.renderTerrain model.displayOptions reducedTrack

                    else
                        []
            in
            updatedScene

        Nothing ->
            []


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
    --if Accordion.tabIsOpen Flythrough.toolLabel model.toolsAccordion then
    --    --if model.flythrough.flythrough /= Nothing then
    --    Sub.batch
    --        [ PortController.messageReceiver PortMessage
    --        , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
    --        , Time.every 50 Tick
    --        , MarkerControls.subscription model.markerOptions MarkerMessage
    --        ]
    --
    --else
    Sub.batch
        [ PortController.messageReceiver PortMessage
        , randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        , MarkerControls.subscription model.markerOptions MarkerMessage
        ]


toolsAccordion : Model -> List (AccordionEntry Msg)
toolsAccordion model =
    [ -- For V2 we see if a single collection works...
      { label = DisplayOptions.toolLabel
      , state = Contracted
      , content = DisplayOptions.viewDisplayOptions model.displayOptions DisplayOptionsMessage
      , info = DisplayOptions.info
      , video = Just "https://youtu.be/N7zGRJvke_M"
      , isFavourite = False
      }

    --, { label = LoopedTrack.toolLabel
    --  , state = Contracted
    --  , content =
    --        LoopedTrack.viewLoopTools
    --            model.displayOptions.imperialMeasure
    --            model.observations.loopiness
    --            model.track
    --            LoopMsg
    --  , info = LoopedTrack.info
    --  , video = Just "https://youtu.be/B3SGh8KhDu0"
    --  , isFavourite = False
    --  }
    --, { label = BendSmoother.toolLabel
    --  , state = Contracted
    --  , content =
    --        BendSmoother.viewBendFixerPane
    --            model.displayOptions.imperialMeasure
    --            model.bendOptions
    --            BendSmoothMessage
    --  , info = BendSmoother.info
    --  , video = Just "https://youtu.be/VO5jsOZmTIg"
    --  , isFavourite = False
    --  }
    --, { label = CurveFormer.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (CurveFormer.view
    --                model.displayOptions.imperialMeasure
    --                model.curveFormer
    --                CurveFormerMsg
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = CurveFormer.info
    --  , video = Just "https://youtu.be/DjdwAFkgw2o"
    --  , isFavourite = False
    --  }
    --, { label = GradientLimiter.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (GradientLimiter.viewGradientLimitPane
    --                model.gradientLimiter
    --                GradientLimiter
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = GradientLimiter.info
    --  , video = Just "https://youtu.be/LtcYi4fzImE"
    --  , isFavourite = False
    --  }
    --, { label = GradientSmoother.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (GradientSmoother.viewGradientFixerPane
    --                model.gradientOptions
    --                GradientMessage
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = GradientSmoother.info
    --  , video = Just "https://youtu.be/YTY2CSl0wo8"
    --  , isFavourite = False
    --  }
    , { label = Nudge.toolLabel
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

    --, { label = MoveAndStretch.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (MoveAndStretch.view
    --                model.displayOptions.imperialMeasure
    --                model.moveAndStretch
    --                TwoWayDragMsg
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = MoveAndStretch.info
    --  , video = Just "https://youtu.be/9ag2iSS4OE8"
    --  , isFavourite = False
    --  }
    --, { label = Straightener.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (Straightener.viewStraightenTools
    --                model.straightenOptions
    --                StraightenMessage
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = Straightener.info
    --  , video = Just "https://youtu.be/MQ67mzShvxg"
    --  , isFavourite = False
    --  }
    --, { label = Interpolate.toolLabel
    --  , state = Contracted
    --  , content =
    --        case model.track of
    --            Just _ ->
    --                Interpolate.viewTools
    --                    model.displayOptions.imperialMeasure
    --                    model.insertOptions
    --                    InsertMessage
    --
    --            Nothing ->
    --                none
    --  , info = Interpolate.info
    --  , video = Just "https://youtu.be/C3chnX2Ij_8"
    --  , isFavourite = False
    --  }
    --, { label = DeletePoints.toolLabel
    --  , state = Contracted
    --  , content = viewDeleteTools model.displayOptions.imperialMeasure model.track DeleteMessage
    --  , info = DeletePoints.info
    --  , video = Nothing
    --  , isFavourite = False
    --  }
    --, { label = Flythrough.toolLabel
    --  , state = Contracted
    --  , content =
    --        Flythrough.flythroughControls
    --            model.displayOptions.imperialMeasure
    --            model.flythrough
    --            FlythroughMessage
    --  , info = Flythrough.info
    --  , video = Just "https://youtu.be/lRukK-do_dE"
    --  , isFavourite = False
    --  }
    --, { label = Filters.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (Filters.viewFilterControls model.filterOptions
    --                FilterMessage
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = Filters.info
    --  , video = Just "https://youtu.be/N48cDi_N_x0"
    --  , isFavourite = False
    --  }
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
    --, { label = RotateRoute.toolLabel
    --  , state = Contracted
    --  , content =
    --        Maybe.map
    --            (RotateRoute.view
    --                model.displayOptions.imperialMeasure
    --                model.rotateOptions
    --                model.lastMapClick
    --                RotateMessage
    --            )
    --            model.track
    --            |> Maybe.withDefault none
    --  , info = RotateRoute.info
    --  , video = Just "https://youtu.be/P602MjJLrZ0"
    --  , isFavourite = False
    --  }
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
    -> Model
    -> Model
addToUndoStack entry model =
    { model
        | undoStack = entry :: List.take 9 model.undoStack
        , redoStack = []
        , changeCounter = model.changeCounter + 1
    }


undo : Model -> Model
undo model =
    case ( model.track, model.undoStack ) of
        ( Just track, entry :: undos ) ->
            let
                ( prefix, middle, suffix ) =
                    entry.undoFunction track

                points =
                    (prefix ++ middle ++ suffix) |> prepareTrackPoints

                oldTrack =
                    { track | trackPoints = points }
            in
            { model
                | undoStack = undos
                , redoStack = entry :: model.redoStack
                , track = Just oldTrack
                , changeCounter = model.changeCounter - 1
            }

        _ ->
            model


redo : Model -> Model
redo model =
    case ( model.track, model.redoStack ) of
        ( Just track, entry :: redos ) ->
            let
                ( prefix, middle, suffix ) =
                    entry.editFunction track

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
