module ViewPane exposing (..)

import About
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input exposing (button)
import FeatherIcons
import List.Extra
import Pixels exposing (Pixels, pixels)
import PostUpdateActions exposing (PostUpdateAction(..))
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, trackPointNearestRay)
import ScenePainterFirst
import ScenePainterMap
import ScenePainterPlan
import ScenePainterProfile
import ScenePainterThird
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (useIcon)
import ViewPureStyles exposing (conditionallyVisible, radioButton, toolRowLayout)
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))


type ViewPaneMessage
    = ChooseViewMode Int ViewingMode
    | ImageMessage Int ScenePainterCommon.ImageMsg
    | AddPane
    | RemovePane Int
    | LinkPane Int Bool
    | ToggleColumns


type ViewPaneAction msg
    = PaneLayoutChange (ViewPane -> ViewPane)
    | ImageAction (PostUpdateActions.PostUpdateAction msg)
    | PaneNoOp


type alias ViewPane =
    { paneId : Int
    , visible : Bool
    , activeContext : ViewingMode
    , thirdPersonContext : ViewingContext
    , firstPersonContext : ViewingContext
    , planContext : ViewingContext
    , profileContext : ViewingContext
    , mapContext : ViewingContext
    , viewPixels : ( Quantity Int Pixels, Quantity Int Pixels )
    , paneLinked : Bool
    , useTwoColumnLayout : Bool
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
    , mapContext = newViewingContext ViewMap
    , viewPixels = ( pixels 800, pixels 500 )
    , paneLinked = True
    , useTwoColumnLayout = False
    }


defaultViewPanes : List ViewPane
defaultViewPanes =
    [ { defaultViewPane | activeContext = ViewAbout }
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


mapOverPanes : (ViewPane -> ViewPane) -> List ViewPane -> List ViewPane
mapOverPanes f panes =
    List.map f panes


mapOverAllContexts : (ViewingContext -> ViewingContext) -> List ViewPane -> List ViewPane
mapOverAllContexts f panes =
    List.map
        (mapOverPaneContexts f)
        panes


mapOverProfileContexts : (ViewingContext -> ViewingContext) -> List ViewPane -> List ViewPane
mapOverProfileContexts f panes =
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
    }


makeMapCommands : Track -> List ViewPane -> List (Cmd msg)
makeMapCommands track viewPanes =
    case List.head viewPanes of
        Just pane ->
            ScenePainterMap.mapTrackHasChanged pane.mapContext track

        Nothing ->
            []


initialiseMap : Track -> List ViewPane -> List (Cmd msg)
initialiseMap track viewPanes =
    case List.head viewPanes of
        Just pane ->
            ScenePainterMap.initialiseMap pane.mapContext track

        Nothing ->
            []


refreshSceneSearcher : Track -> ViewingContext -> ViewingContext
refreshSceneSearcher track context =
    -- We refresh this if the track changes; the closure makes
    -- the latest version of Track available for searching.
    case context.viewingMode of
        ViewThirdPerson ->
            { context | sceneSearcher = trackPointNearestRay track.trackPoints }

        ViewFirstPerson ->
            { context | sceneSearcher = trackPointNearestRay track.trackPoints }

        ViewProfile ->
            { context | sceneSearcher = ScenePainterProfile.profilePointNearestRay track.trackPoints }

        ViewPlan ->
            { context | sceneSearcher = trackPointNearestRay track.trackPoints }

        ViewMap ->
            context

        ViewAbout ->
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
                , Input.optionWith ViewMap <| radioButton "Map"
                , Input.optionWith ViewAbout <| radioButton "?"
                ]

            else
                [ Input.optionWith ViewThirdPerson <| radioButton "Third person"
                , Input.optionWith ViewFirstPerson <| radioButton "First person"
                , Input.optionWith ViewPlan <| radioButton "Plan"
                , Input.optionWith ViewProfile <| radioButton "Profile"
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
                List.take 4 fullOptionList
        }


view : ( Scene, Scene ) -> DisplayOptions -> (ViewPaneMessage -> msg) -> ViewPane -> Element msg
view ( scene, profile ) options wrapper pane =
    -- The layout logic is complicated as the result of much
    -- experimentation to make the map behave predictably.
    -- Further complicated by Map sketch mode.
    if pane.visible then
        column [ ]
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
                            options
                            scene
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewFirstPerson ->
                        ScenePainterFirst.viewScene
                            (pane.activeContext == ViewFirstPerson)
                            (getActiveContext pane)
                            options
                            scene
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewPlan ->
                        ScenePainterPlan.viewScene
                            (pane.activeContext == ViewPlan)
                            (getActiveContext pane)
                            scene
                            (imageMessageWrapper pane.paneId >> wrapper)

                    ViewProfile ->
                        ScenePainterProfile.viewScene
                            (pane.activeContext == ViewProfile)
                            (getActiveContext pane)
                            options
                            profile
                            (imageMessageWrapper pane.paneId >> wrapper)

                    _ ->
                        About.viewAboutText
                            pane.thirdPersonContext

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
    -> List ViewPane
    -> (ViewPaneMessage -> msg)
    -> ( Maybe ViewPane, ViewPaneAction (Cmd msg) )
update msg panes wrap =
    case msg of
        ChooseViewMode paneId mode ->
            let
                currentPane =
                    List.Extra.getAt paneId panes
            in
            case currentPane of
                Just pane ->
                    ( Just { pane | activeContext = mode }
                    , ImageAction ActionRepaintMap
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
                                        (wrap << imageMessageWrapper pane.paneId)
                            in
                            ( Just { pane | profileContext = newContext }
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
            ( paneMadeVisible, PaneNoOp )

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
            ( paneHidden, PaneNoOp )

        LinkPane id isLinked ->
            let
                paneLinked =
                    Maybe.map
                        (\pane -> { pane | paneLinked = isLinked })
                        (List.Extra.getAt id panes)
            in
            ( paneLinked, PaneNoOp )

        ToggleColumns ->
            ( Nothing
            , PaneLayoutChange toggleTwoColumnLayout
            )


updatePointerInLinkedPanes : TrackPoint -> ViewPane -> ViewPane
updatePointerInLinkedPanes tp pane =
    if pane.paneLinked then
        { pane
            | thirdPersonContext = ScenePainterCommon.changeFocusTo tp pane.thirdPersonContext
            , firstPersonContext = ScenePainterCommon.changeFocusTo tp pane.firstPersonContext
            , planContext = ScenePainterCommon.changeFocusTo tp pane.planContext
            , profileContext = ScenePainterProfile.changeFocusTo tp pane.profileContext
            , mapContext = ScenePainterCommon.changeFocusTo tp pane.mapContext
        }

    else
        pane


mapPaneIsLinked : List ViewPane -> Bool
mapPaneIsLinked panes =
    List.head panes |> Maybe.map .paneLinked |> Maybe.withDefault False
