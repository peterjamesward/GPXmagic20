module ViewPane exposing (..)

import About
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input exposing (button)
import FeatherIcons
import GeoCodeDecoders exposing (IpInfo)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Pixels exposing (Pixels, pixels)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import ScenePainterCommon exposing (ImageMsg(..))
import ScenePainterFirst
import ScenePainterMap
import ScenePainterPlan
import ScenePainterProfile
import ScenePainterProfileCharts
import ScenePainterThird
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import TrackSearchQueries exposing (trackPointNearestFromIndexFor3d, trackPointNearestFromIndexForPlan)
import Utils exposing (useIcon)
import ViewPureStyles exposing (conditionallyVisible, radioButton)
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))


type ViewPaneMessage
    = ChooseViewMode Int ViewingMode
    | ImageMessage Int ScenePainterCommon.ImageMsg
    | AddPane
    | RemovePane Int
    | LinkPane Int Bool
    | ToggleColumns


type ViewPaneAction trck msg
    = PaneLayoutChange (ViewPane -> ViewPane)
    | ImageAction (PostUpdateActions.PostUpdateAction trck msg)
    | PaneNoOp


type alias ViewPane =
    { paneId : Int
    , visible : Bool
    , activeContext : ViewingMode
    , thirdPersonContext : ViewingContext
    , firstPersonContext : ViewingContext
    , planContext : ViewingContext
    , profileContext : ViewingContext
    , profileChartContext : ViewingContext
    , mapContext : ViewingContext
    , viewPixels : ( Quantity Int Pixels, Quantity Int Pixels )
    , paneLinked : Bool
    , useTwoColumnLayout : Bool
    }


type alias SavedPaneState =
    -- This is all we need to restore views; split location is elsewhere.
    { twoColumns : Bool
    , views : List String
    }


defaultViewPane : ViewPane
defaultViewPane =
    { paneId = 0
    , visible = True
    , activeContext = ViewThirdPerson
    , thirdPersonContext = newViewingContext ViewThirdPerson
    , firstPersonContext = newViewingContext ViewFirstPerson
    , planContext = newViewingContext ViewPlan
    , profileContext = newViewingContext ViewProfile
    , profileChartContext = newViewingContext ViewProfileCharts
    , mapContext = newViewingContext ViewMap
    , viewPixels = ( pixels 800, pixels 500 )
    , paneLinked = True
    , useTwoColumnLayout = False
    }


viewPanesWhenNoTrack : List ViewPane
viewPanesWhenNoTrack =
    [ { defaultViewPane | activeContext = ViewAbout }
    , { defaultViewPane | paneId = 1, visible = False }
    , { defaultViewPane | paneId = 2, visible = False }
    , { defaultViewPane | paneId = 3, visible = False }
    ]


viewPanesWithTrack : List ViewPane
viewPanesWithTrack =
    [ { defaultViewPane | activeContext = ViewThirdPerson }
    , { defaultViewPane | paneId = 1, visible = False }
    , { defaultViewPane | paneId = 2, visible = False }
    , { defaultViewPane | paneId = 3, visible = False }
    ]


info =
    """## Views

Enlarge or reduce the size of the view panes.

Add another view pane, to a maximum of four.

Use the Close (X) above a view pane to close it.

The padlock symbol above each pane determines whether or not it
automatically re-centres when you double click on any pane or
change the orange pointer position with the controls."""



-- Here some helpers for managing the list of panes.


updateViewPanes : Maybe ViewPane -> List ViewPane -> List ViewPane
updateViewPanes pane panes =
    case pane of
        Just isPane ->
            if isPane.paneId < List.length panes then
                List.Extra.updateAt isPane.paneId (always isPane) panes

            else
                panes ++ [ { isPane | paneId = List.length panes } ]

        Nothing ->
            panes


mapOverPanes : (ViewPane -> a) -> List ViewPane -> List a
mapOverPanes f panes =
    List.map f panes


isViewingModeVisible : ViewingMode -> List ViewPane -> Bool
isViewingModeVisible mode panes =
    -- This will allow us to make Scene evaluation lazy.
    panes |> List.any (\p -> p.activeContext == mode && p.visible)


isMapVisible panes =
    -- Helper
    isViewingModeVisible ViewMap panes


isProfileVisible panes =
    -- Helper
    isViewingModeVisible ViewProfile panes
        || isViewingModeVisible ViewProfileCharts panes


is3dVisible panes =
    -- Helper
    isViewingModeVisible ViewFirstPerson panes
        || isViewingModeVisible ViewThirdPerson panes
        || isViewingModeVisible ViewPlan panes


mapOverAllContexts : (ViewingContext -> ViewingContext) -> List ViewPane -> List ViewPane
mapOverAllContexts f panes =
    List.map
        (mapOverPaneContexts f)
        panes


mapOverProfileContexts : (ViewingContext -> ViewingContext) -> List ViewPane -> List ViewPane
mapOverProfileContexts f panes =
    -- Need this if the exaggeration changes, which is naff, really.
    List.map
        (\pane -> { pane | profileContext = f pane.profileContext })
        panes


mapOverPaneContexts : (ViewingContext -> ViewingContext) -> ViewPane -> ViewPane
mapOverPaneContexts f pane =
    { pane
        | thirdPersonContext = f pane.thirdPersonContext
        , firstPersonContext = f pane.firstPersonContext
        , planContext = f pane.planContext
        , profileContext = f pane.profileContext
        , mapContext = f pane.mapContext
        , profileChartContext = f pane.profileChartContext
    }


setSize size context =
    { context | size = size }


toggleTwoColumnLayout : ViewPane -> ViewPane
toggleTwoColumnLayout pane =
    { pane | useTwoColumnLayout = not pane.useTwoColumnLayout }


setViewPaneSize : Int -> ViewPane -> ViewPane
setViewPaneSize split pane =
    let
        newSize =
            if pane.useTwoColumnLayout then
                ( pixels <| (split - 40) // 2
                , pixels <| (split - 40) // 2 * 500 // 800
                )

            else
                ( pixels <| split - 40
                , pixels <| (split - 40) * 500 // 800
                )
    in
    { pane | viewPixels = newSize }
        |> mapOverPaneContexts (setSize newSize)


resetAllViews :
    Track
    -> ViewPane
    -> ViewPane
resetAllViews track pane =
    { pane
        | thirdPersonContext = ScenePainterThird.initialiseView pane.viewPixels track pane.thirdPersonContext
        , firstPersonContext = ScenePainterFirst.initialiseView pane.viewPixels track pane.firstPersonContext
        , planContext = ScenePainterPlan.initialiseView pane.viewPixels track pane.planContext
        , profileContext = ScenePainterProfile.initialiseView pane.viewPixels track pane.profileContext
        , mapContext = ScenePainterMap.initialiseView pane.viewPixels track pane.mapContext
        , profileChartContext = ScenePainterProfileCharts.initialiseView pane.viewPixels track pane.profileChartContext
    }


makeMapCommands : Track -> List ViewPane -> List E.Value -> Cmd msg
makeMapCommands track viewPanes previews =
    if isMapVisible viewPanes then
        case List.head viewPanes of
            Just pane ->
                Cmd.batch <| ScenePainterMap.mapTrackHasChanged pane.mapContext track previews

            Nothing ->
                Cmd.none

    else
        Cmd.none


initialiseMap : Track -> List ViewPane -> Cmd msg
initialiseMap track viewPanes =
    case List.head viewPanes of
        Just pane ->
            ScenePainterMap.initialiseMap pane.mapContext track

        Nothing ->
            Cmd.none


refreshSceneSearcher : Track -> ViewingContext -> ViewingContext
refreshSceneSearcher track context =
    -- We refresh this if the track changes; the closure makes
    -- the latest version of Track available for searching.
    -- Assuming these don't make deep copies of the index!
    case context.viewingMode of
        ViewThirdPerson ->
            { context | sceneSearcher = trackPointNearestFromIndexFor3d track.spatialIndex }

        ViewFirstPerson ->
            { context | sceneSearcher = trackPointNearestFromIndexFor3d track.spatialIndex }

        ViewProfile ->
            { context | sceneSearcher = ScenePainterProfile.profilePointNearestRay track.trackPoints }

        ViewPlan ->
            { context | sceneSearcher = trackPointNearestFromIndexForPlan track.spatialIndex }

        ViewMap ->
            context

        ViewAbout ->
            context

        ViewProfileCharts ->
            context


getActiveContext : ViewPane -> ViewingContext
getActiveContext pane =
    case pane.activeContext of
        ViewThirdPerson ->
            pane.thirdPersonContext

        ViewFirstPerson ->
            pane.firstPersonContext

        ViewProfile ->
            pane.profileContext

        ViewPlan ->
            pane.planContext

        ViewMap ->
            pane.mapContext

        ViewAbout ->
            pane.thirdPersonContext

        ViewProfileCharts ->
            pane.profileChartContext


imageMessageWrapper : Int -> ImageMsg -> ViewPaneMessage
imageMessageWrapper paneId m =
    ImageMessage paneId m


viewModeChoices : ViewPane -> (ViewPaneMessage -> msg) -> Element msg
viewModeChoices pane wrapper =
    let
        ( w, h ) =
            pane.viewPixels

        fullOptionList =
            if w |> Quantity.lessThan (pixels 720) then
                [ Input.optionWith ViewThirdPerson <| radioButton "3rd"
                , Input.optionWith ViewFirstPerson <| radioButton "1st"
                , Input.optionWith ViewPlan <| radioButton "Plan"
                , Input.optionWith ViewProfile <| radioButton "Prof."
                , Input.optionWith ViewProfileCharts <| radioButton "Chart"
                , Input.optionWith ViewMap <| radioButton "Map"
                , Input.optionWith ViewAbout <| radioButton "?"
                ]

            else
                [ Input.optionWith ViewThirdPerson <| radioButton "Third person"
                , Input.optionWith ViewFirstPerson <| radioButton "First person"
                , Input.optionWith ViewPlan <| radioButton "Plan"
                , Input.optionWith ViewProfile <| radioButton "Profile"
                , Input.optionWith ViewProfileCharts <| radioButton "Charts"
                , Input.optionWith ViewMap <| radioButton "Map"
                , Input.optionWith ViewAbout <| radioButton "About"
                ]
    in
    Input.radioRow
        [ Border.rounded 6 ]
        { onChange = ChooseViewMode pane.paneId >> wrapper
        , selected = Just pane.activeContext
        , label =
            Input.labelHidden "Choose view"
        , options =
            if pane.paneId == 0 then
                fullOptionList

            else
                List.take 5 fullOptionList
        }


view :
    ( Scene, Scene, Scene )
    ->
        { model
            | displayOptions : DisplayOptions
            , ipInfo : Maybe IpInfo
            , track : Maybe Track
        }
    -> (ViewPaneMessage -> msg)
    -> ViewPane
    -> Element msg
view ( scene, profile, plan ) { displayOptions, ipInfo, track } wrapper pane =
    -- The layout logic is complicated as the result of much
    -- experimentation to make the map behave predictably.
    -- Essentially, do not create and destroy the map DIV.
    -- Further complicated by Map sketch mode.
    if pane.visible then
        column []
            [ if List.length scene > 0 then
                row [ width fill, spacingXY 10 0 ]
                    [ if pane.paneId == 0 then
                        viewPaneTools wrapper

                      else
                        none
                    , viewModeChoices pane wrapper
                    , viewPaneControls pane wrapper
                    ]

              else
                Input.radioRow
                    [ Border.rounded 6 ]
                    { onChange = ChooseViewMode pane.paneId >> wrapper
                    , selected = Just ViewAbout
                    , label = Input.labelHidden "Choose view"
                    , options = [ Input.optionWith ViewAbout <| radioButton "About" ]
                    }
            , conditionallyVisible (pane.activeContext /= ViewMap) <|
                case pane.activeContext of
                    ViewThirdPerson ->
                        ScenePainterThird.viewScene
                            (pane.activeContext == ViewThirdPerson)
                            (getActiveContext pane)
                            displayOptions
                            scene
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewFirstPerson ->
                        ScenePainterFirst.viewScene
                            (pane.activeContext == ViewFirstPerson)
                            (getActiveContext pane)
                            displayOptions
                            scene
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewPlan ->
                        ScenePainterPlan.viewScene
                            (pane.activeContext == ViewPlan)
                            (getActiveContext pane)
                            plan
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewProfile ->
                        ScenePainterProfile.viewScene
                            (pane.activeContext == ViewProfile)
                            (getActiveContext pane)
                            displayOptions
                            profile
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewProfileCharts ->
                            none
                        --case track of
                        --    Just aTrack ->
                        --        ScenePainterProfileCharts.viewScene
                        --            (pane.activeContext == ViewProfileCharts)
                        --            (getActiveContext pane)
                        --            displayOptions
                        --            (imageMessageWrapper pane.paneId >> wrapper)
                        --
                        --    Nothing ->
                        --        text "Where's the track gone?"

                    ViewMap ->
                        About.viewAboutText
                            pane.thirdPersonContext
                            ipInfo

                    ViewAbout ->
                        About.viewAboutText
                            pane.thirdPersonContext
                            ipInfo

            -- We leave the Map DIV intact, as destroying and creating is APITA.
            , conditionallyVisible (pane.activeContext == ViewMap) <|
                ScenePainterMap.viewScene
                    (pane.activeContext == ViewMap)
                    (getActiveContext pane)
                    []
                    (imageMessageWrapper pane.paneId >> wrapper)
            ]

    else
        none


viewPaneTools : (ViewPaneMessage -> msg) -> Element msg
viewPaneTools wrap =
    let
        makeButton cmd icon =
            button
                []
                { onPress = Just <| wrap cmd
                , label = useIcon icon
                }

        toggleColumnLayout =
            makeButton ToggleColumns FeatherIcons.columns

        addButton =
            makeButton AddPane FeatherIcons.copy
    in
    row [ spacingXY 10 0 ]
        [ toggleColumnLayout
        , addButton
        ]


viewPaneControls : ViewPane -> (ViewPaneMessage -> msg) -> Element msg
viewPaneControls pane wrap =
    let
        makeButton cmd icon =
            button
                []
                { onPress = Just <| wrap cmd
                , label = useIcon icon
                }

        removeButton id =
            makeButton (RemovePane id) FeatherIcons.xSquare

        linkButton id =
            if pane.paneLinked then
                makeButton (LinkPane id False) FeatherIcons.lock

            else
                makeButton (LinkPane id True) FeatherIcons.unlock
    in
    row [ alignRight ] <|
        if pane.paneId == 0 then
            [ linkButton pane.paneId
            ]

        else
            [ linkButton pane.paneId
            , removeButton pane.paneId
            ]


update :
    ViewPaneMessage
    -> DisplayOptions
    -> List ViewPane
    -> (ViewPaneMessage -> msg)
    -> Track
    -> ( Maybe ViewPane, ViewPaneAction trck (Cmd msg) )
update msg options panes wrap track =
    case msg of
        ChooseViewMode paneId mode ->
            let
                currentPane =
                    List.Extra.getAt paneId panes
            in
            case currentPane of
                Just pane ->
                    ( Just { pane | activeContext = mode }
                    , PaneLayoutChange identity
                    )

                Nothing ->
                    ( Nothing, PaneNoOp )

        ImageMessage paneId imageMsg ->
            let
                currentPane =
                    List.Extra.getAt paneId panes
            in
            case currentPane of
                Just pane ->
                    case pane.activeContext of
                        ViewThirdPerson ->
                            let
                                ( newContext, action ) =
                                    ScenePainterThird.update
                                        imageMsg
                                        pane.thirdPersonContext
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | thirdPersonContext = newContext }
                            , ImageAction action
                            )

                        ViewFirstPerson ->
                            let
                                ( newContext, action ) =
                                    ScenePainterFirst.update
                                        imageMsg
                                        pane.firstPersonContext
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | firstPersonContext = newContext }
                            , ImageAction action
                            )

                        ViewPlan ->
                            let
                                ( newContext, action ) =
                                    ScenePainterPlan.update
                                        imageMsg
                                        pane.planContext
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | planContext = newContext }
                            , ImageAction action
                            )

                        ViewProfile ->
                            let
                                ( newContext, action ) =
                                    ScenePainterProfile.update
                                        imageMsg
                                        pane.profileContext
                                        options
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | profileContext = newContext }
                            , ImageAction action
                            )

                        ViewProfileCharts ->
                            let
                                ( newContext, action ) =
                                    ScenePainterProfileCharts.update
                                        imageMsg
                                        pane.profileChartContext
                                        options
                                        (wrap << imageMessageWrapper pane.paneId)
                                        track
                            in
                            ( Just { pane | profileChartContext = newContext }
                            , ImageAction action
                            )

                        ViewMap ->
                            let
                                ( newContext, action ) =
                                    ScenePainterMap.update
                                        imageMsg
                                        pane.mapContext
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | mapContext = newContext }
                            , ImageAction action
                            )

                        _ ->
                            ( Just pane, PaneNoOp )

                Nothing ->
                    ( Nothing, PaneNoOp )

        AddPane ->
            -- Make visible first non-visible pane
            let
                firstHiddenPane =
                    List.Extra.find (not << .visible) panes

                paneMadeVisible =
                    Maybe.map (\pane -> { pane | visible = True }) firstHiddenPane
            in
            ( paneMadeVisible
            , PaneLayoutChange identity
            )

        RemovePane id ->
            let
                currentPane =
                    if id > 0 then
                        List.Extra.getAt id panes

                    else
                        Nothing

                paneHidden =
                    Maybe.map (\pane -> { pane | visible = False }) currentPane
            in
            ( paneHidden
            , PaneLayoutChange identity
            )

        LinkPane id isLinked ->
            let
                paneLinked =
                    Maybe.map
                        (\pane -> { pane | paneLinked = isLinked })
                        (List.Extra.getAt id panes)
            in
            ( paneLinked
            , PaneNoOp
            )

        ToggleColumns ->
            ( Nothing
            , PaneLayoutChange toggleTwoColumnLayout
            )


updatePointerInLinkedPanes : Track -> ViewPane -> ViewPane
updatePointerInLinkedPanes track pane =
    if pane.paneLinked then
        { pane
            | thirdPersonContext = ScenePainterCommon.changeFocusTo track pane.thirdPersonContext
            , firstPersonContext = ScenePainterCommon.changeFocusTo track pane.firstPersonContext
            , planContext = ScenePainterCommon.changeFocusTo track pane.planContext
            , profileContext = ScenePainterProfile.changeFocusTo track pane.profileContext
            , mapContext = ScenePainterCommon.changeFocusTo track pane.mapContext
            , profileChartContext =
                if pane.visible then
                    ScenePainterProfileCharts.changeFocusTo track pane.profileChartContext

                else
                    pane.profileChartContext
        }

    else
        pane


mapPaneIsLinked : List ViewPane -> Bool
mapPaneIsLinked panes =
    List.head panes |> Maybe.map .paneLinked |> Maybe.withDefault False


storePaneLayout : List ViewPane -> E.Value
storePaneLayout panes =
    let
        storageFormat : SavedPaneState
        storageFormat =
            { twoColumns = List.any .useTwoColumnLayout panes
            , views = List.map (encodeView << .activeContext) <| List.filter .visible panes
            }

        encodeView : ViewingMode -> String
        encodeView v =
            case v of
                ViewThirdPerson ->
                    "third"

                ViewFirstPerson ->
                    "first"

                ViewProfile ->
                    "profile"

                ViewProfileCharts ->
                    "charts"

                ViewPlan ->
                    "plan"

                ViewMap ->
                    "map"

                ViewAbout ->
                    "about"
    in
    E.object
        [ ( "twocolumns", E.bool storageFormat.twoColumns )
        , ( "views", E.list E.string storageFormat.views )
        ]


restorePaneState : E.Value -> List ViewPane -> List ViewPane
restorePaneState saved viewPanes =
    let
        storage =
            D.decodeValue storageFormatDecode saved

        decodeView s =
            case s of
                "third" ->
                    ViewThirdPerson

                "first" ->
                    ViewFirstPerson

                "profile" ->
                    ViewProfile

                "charts" ->
                    ViewProfileCharts

                "plan" ->
                    ViewPlan

                "map" ->
                    ViewMap

                _ ->
                    ViewAbout
    in
    case storage of
        Ok recovered ->
            let
                ( visible, hidden ) =
                    List.Extra.splitAt (List.length recovered.views) viewPanes
            in
            List.map2
                (\original mode ->
                    { original
                        | visible = True
                        , activeContext = decodeView mode
                        , useTwoColumnLayout = recovered.twoColumns
                    }
                )
                visible
                recovered.views
                ++ List.map
                    (\v ->
                        { v
                            | visible = False
                            , useTwoColumnLayout = recovered.twoColumns
                        }
                    )
                    hidden

        _ ->
            viewPanes


storageFormatDecode =
    D.map2 SavedPaneState
        (D.field "twocolumns" D.bool)
        (D.field "views" (D.list D.string))
