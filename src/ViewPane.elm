module ViewPane exposing (..)

import ColourPalette exposing (radioButtonDefault, radioButtonSelected, radioButtonText)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import Html.Attributes exposing (id, style)
import ImagePostUpdateActions exposing (PostUpdateAction(..))
import List.Extra
import Pixels exposing (Pixels, pixels)
import Quantity exposing (Quantity)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, trackPointNearestRay)
import ScenePainterMap
import ScenePainterPlan
import ScenePainterProfile
import ScenePainterThird
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (useIcon)
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))


type ViewPaneMessage
    = ChooseViewMode Int ViewingMode
    | ImageMessage Int ScenePainterCommon.ImageMsg
    | AddPane
    | RemovePane Int
    | EnlargePanes
    | DiminishPanes
    | LinkPane Int Bool


type ViewPaneAction
    = ApplyToAllPanes (ViewPane -> ViewPane)
    | ImageAction ImagePostUpdateActions.PostUpdateAction
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
    , viewPixels = ( pixels 800, pixels 600 )
    , paneLinked = True
    }



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
    panes
        |> List.map
            (\pane ->
                { pane
                    | thirdPersonContext = f pane.thirdPersonContext
                    , firstPersonContext = f pane.firstPersonContext
                    , planContext = f pane.planContext
                    , profileContext = f pane.profileContext
                    , mapContext = f pane.mapContext
                }
            )


mapOverPaneContexts : (ViewingContext -> ViewingContext) -> ViewPane -> ViewPane
mapOverPaneContexts f pane =
    { pane
        | thirdPersonContext = f pane.thirdPersonContext
        , firstPersonContext = f pane.firstPersonContext
        , planContext = f pane.planContext
        , profileContext = f pane.profileContext
        , mapContext = f pane.mapContext
    }


enlargePane : ViewPane -> ViewPane
enlargePane pane =
    let
        enlargeContext context =
            let
                ( width, height ) =
                    context.size
            in
            { context
                | size =
                    ( width |> Quantity.plus (pixels 80)
                    , height |> Quantity.plus (pixels 60)
                    )
            }
    in
    pane |> mapOverPaneContexts enlargeContext


diminishPane : ViewPane -> ViewPane
diminishPane pane =
    let
        diminishContext context =
            let
                ( width, height ) =
                    context.size
            in
            { context
                | size =
                    ( width |> Quantity.minus (pixels 80)
                    , height |> Quantity.minus (pixels 60)
                    )
            }
    in
    pane |> mapOverPaneContexts diminishContext


resetAllViews :
    Track
    -> ViewPane
    -> ViewPane
resetAllViews track pane =
    let
        newPane =
            { pane
                | thirdPersonContext = ScenePainterThird.initialiseView pane.viewPixels track.track
                , firstPersonContext = ScenePainterThird.initialiseView pane.viewPixels track.track
                , planContext = ScenePainterPlan.initialiseView pane.viewPixels track.track
                , profileContext = ScenePainterProfile.initialiseView pane.viewPixels track.track
                , mapContext = ScenePainterMap.initialiseView pane.viewPixels track.track
            }
    in
    newPane


makeMapCommands : Track -> List ViewPane -> List (Cmd msg)
makeMapCommands track viewPanes =
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
            { context | sceneSearcher = trackPointNearestRay track.track }

        ViewFirstPerson ->
            { context | sceneSearcher = trackPointNearestRay track.track }

        ViewProfile ->
            { context | sceneSearcher = ScenePainterProfile.profilePointNearestRay track.track }

        ViewPlan ->
            { context | sceneSearcher = trackPointNearestRay track.track }

        ViewMap ->
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


imageMessageWrapper : Int -> ImageMsg -> ViewPaneMessage
imageMessageWrapper paneId m =
    ImageMessage paneId m


viewModeChoices : ViewPane -> (ViewPaneMessage -> msg) -> Element msg
viewModeChoices pane wrapper =
    let
        fullOptionList =
            [ Input.optionWith ViewThirdPerson <| radioButton "Third person"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            , Input.optionWith ViewMap <| radioButton "Map"
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
                List.take 3 fullOptionList
        }


radioButton label state =
    el
        [ padding 10
        , spacing 2
        , Border.widthEach { left = 2, right = 2, top = 2, bottom = 0 }
        , Border.roundEach { topLeft = 10, bottomLeft = 0, topRight = 10, bottomRight = 0 }
        , Background.color <|
            if state == Input.Selected then
                radioButtonSelected

            else
                radioButtonDefault
        , Font.color radioButtonText
        , Font.size 16
        ]
    <|
        el [ centerX, centerY ] <|
            text label


conditionallyVisible : Bool -> Element msg -> Element msg
conditionallyVisible test element =
    -- This turns out to be the secret sauce for easier map integration.
    -- It means we can pre-load a Mapbox map element.
    if test then
        el [] element

    else
        el [ htmlAttribute (style "display" "none") ] element


view : ( Scene, Scene ) -> (ViewPaneMessage -> msg) -> ViewPane -> Element msg
view ( scene, profile ) wrapper pane =
    conditionallyVisible pane.visible <|
        column [ paddingEach { top = 5, bottom = 5, left = 0, right = 0 } ]
            [ row [ width fill ]
                [ el [ alignLeft ] <| viewModeChoices pane wrapper
                , viewPaneControls pane wrapper
                ]
            , conditionallyVisible (pane.activeContext == ViewThirdPerson) <|
                ScenePainterThird.viewScene
                    (getActiveContext pane)
                    scene
                    (imageMessageWrapper pane.paneId >> wrapper)
            , conditionallyVisible (pane.activeContext == ViewPlan) <|
                ScenePainterPlan.viewScene
                    (getActiveContext pane)
                    scene
                    (imageMessageWrapper pane.paneId >> wrapper)
            , conditionallyVisible (pane.activeContext == ViewProfile) <|
                ScenePainterProfile.viewScene
                    (getActiveContext pane)
                    profile
                    (imageMessageWrapper pane.paneId >> wrapper)
            , conditionallyVisible (pane.activeContext == ViewMap) <|
                ScenePainterMap.viewScene
                    (getActiveContext pane)
                    profile
                    (imageMessageWrapper pane.paneId >> wrapper)
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

        enlargeButton =
            makeButton EnlargePanes FeatherIcons.maximize2

        diminishButton =
            makeButton DiminishPanes FeatherIcons.minimize2

        addButton =
            makeButton AddPane FeatherIcons.copy

        removeButton id =
            makeButton (RemovePane id) FeatherIcons.xSquare

        linkButton id =
            if pane.paneLinked then
                makeButton (LinkPane id False) FeatherIcons.lock

            else
                makeButton (LinkPane id True) FeatherIcons.unlock
    in
    row [ spacing 10, alignRight, moveLeft 50 ] <|
        if pane.paneId == 0 then
            [ linkButton pane.paneId
            , enlargeButton
            , diminishButton
            , addButton
            ]

        else
            [ linkButton pane.paneId
            , enlargeButton
            , diminishButton
            , addButton
            , removeButton pane.paneId
            ]


update : ViewPaneMessage -> List ViewPane -> Time.Posix -> ( Maybe ViewPane, ViewPaneAction )
update msg panes now =
    case msg of
        ChooseViewMode paneId mode ->
            let
                currentPane =
                    List.Extra.getAt paneId panes
            in
            case currentPane of
                Just pane ->
                    ( Just { pane | activeContext = mode }, PaneNoOp )

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
                                    ScenePainterThird.update imageMsg pane.thirdPersonContext now
                            in
                            ( Just { pane | thirdPersonContext = newContext }
                            , ImageAction action
                            )

                        ViewPlan ->
                            let
                                ( newContext, action ) =
                                    ScenePainterPlan.update imageMsg pane.planContext now
                            in
                            ( Just { pane | planContext = newContext }
                            , ImageAction action
                            )

                        ViewProfile ->
                            let
                                ( newContext, action ) =
                                    ScenePainterProfile.update imageMsg pane.profileContext now
                            in
                            ( Just { pane | profileContext = newContext }
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

        EnlargePanes ->
            ( Nothing
            , ApplyToAllPanes enlargePane
            )

        DiminishPanes ->
            ( Nothing
            , ApplyToAllPanes diminishPane
            )

        LinkPane id isLinked ->
            let
                currentPane =
                    if id > 0 then
                        List.Extra.getAt id panes

                    else
                        Nothing

                paneLinked =
                    Maybe.map (\pane -> { pane | paneLinked = isLinked }) currentPane
            in
            ( paneLinked, PaneNoOp )


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
