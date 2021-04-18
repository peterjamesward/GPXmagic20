module ViewPane exposing (..)

import ColourPalette exposing (radioButtonDefault, radioButtonSelected, radioButtonText)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import List.Extra
import Pixels exposing (Pixels, pixels)
import Quantity exposing (Quantity)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, PostUpdateAction(..), trackPointNearestRay)
import ScenePainterPlan
import ScenePainterProfile
import ScenePainterThird
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (useIcon)
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))


type alias ViewPane =
    { paneId : Int
    , visible : Bool
    , activeContext : ViewingMode
    , thirdPersonContext : ViewingContext
    , firstPersonContext : ViewingContext
    , planContext : ViewingContext
    , profileContext : ViewingContext
    , viewPixels : ( Quantity Int Pixels, Quantity Int Pixels )
    }


defaultViewPane =
    { paneId = 0
    , visible = True
    , activeContext = ViewThirdPerson
    , thirdPersonContext = newViewingContext ViewThirdPerson
    , firstPersonContext = newViewingContext ViewFirstPerson
    , planContext = newViewingContext ViewPlan
    , profileContext = newViewingContext ViewProfile
    , viewPixels = ( pixels 800, pixels 600 )
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


mapOverPanes : (ViewingContext -> ViewingContext) -> List ViewPane -> List ViewPane
mapOverPanes contextUpdate panes =
    panes
        |> List.map
            (\pane ->
                { pane
                    | thirdPersonContext = contextUpdate pane.thirdPersonContext
                    , firstPersonContext = contextUpdate pane.firstPersonContext
                    , planContext = contextUpdate pane.planContext
                    , profileContext = contextUpdate pane.profileContext
                }
            )


enlargePane : ViewingContext -> ViewingContext
enlargePane context =
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


diminishPane : ViewingContext -> ViewingContext
diminishPane context =
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


resetAllViews :
    List TrackPoint
    -> ViewPane
    -> ViewPane
resetAllViews track pane =
    { pane
        | thirdPersonContext = ScenePainterThird.initialiseView pane.viewPixels track
        , firstPersonContext = ScenePainterThird.initialiseView pane.viewPixels track
        , planContext = ScenePainterPlan.initialiseView pane.viewPixels track
        , profileContext = ScenePainterProfile.initialiseView pane.viewPixels track
    }


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
            pane.planContext


type ViewPaneMessage
    = ChooseViewMode Int ViewingMode
    | ImageMessage Int ScenePainterCommon.ImageMsg
    | AddPane
    | RemovePane Int
    | EnlargePanes
    | DiminishPanes


imageMessageWrapper : Int -> ImageMsg -> ViewPaneMessage
imageMessageWrapper paneId m =
    ImageMessage paneId m


viewModeChoices : ViewPane -> (ViewPaneMessage -> msg) -> Element msg
viewModeChoices pane wrapper =
    Input.radioRow
        [ Border.rounded 6 ]
        { onChange = ChooseViewMode pane.paneId >> wrapper
        , selected = Just pane.activeContext
        , label =
            Input.labelHidden "Choose view"
        , options =
            [ Input.optionWith ViewThirdPerson <| radioButton "Third person"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            ]
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


view : ( Scene, Scene ) -> (ViewPaneMessage -> msg) -> ViewPane -> Element msg
view ( scene, profile ) wrapper pane =
    if pane.visible then
        column [ paddingEach { top = 5, bottom = 5, left = 0, right = 0 } ]
            [ row [ width fill ]
                [ el [ alignLeft ] <| viewModeChoices pane wrapper
                , viewAddAndRemove pane wrapper
                ]
            , viewScene
                ( scene, profile )
                (imageMessageWrapper pane.paneId >> wrapper)
                (getActiveContext pane)
            ]

    else
        none


viewScene : ( Scene, Scene ) -> (ImageMsg -> msg) -> ViewingContext -> Element msg
viewScene ( scene, profile ) wrapper context =
    case context.viewingMode of
        ViewThirdPerson ->
            ScenePainterThird.viewScene context scene wrapper

        ViewPlan ->
            ScenePainterPlan.viewScene context scene wrapper

        ViewProfile ->
            ScenePainterProfile.viewScene context profile wrapper

        _ ->
            fallbackScenePainter context scene wrapper


fallbackScenePainter _ _ _ =
    text "Any day now!"


viewAddAndRemove : ViewPane -> (ViewPaneMessage -> msg) -> Element msg
viewAddAndRemove pane wrap =
    row
        [ spacing 10
        , alignRight
        , moveLeft 50
        ]
        [ if pane.paneId == 0 then
            button
                []
                { onPress = Just <| wrap EnlargePanes
                , label = useIcon FeatherIcons.maximize2
                }

          else
            none
        , if pane.paneId == 0 then
            button
                []
                { onPress = Just <| wrap DiminishPanes
                , label = useIcon FeatherIcons.minimize2
                }

          else
            none
        , button
            []
            { onPress = Just <| wrap AddPane
            , label = useIcon FeatherIcons.copy
            }
        , if pane.paneId > 0 then
            button
                []
                { onPress = Just <| wrap (RemovePane pane.paneId)
                , label = useIcon FeatherIcons.xSquare
                }

          else
            none
        ]


update : ViewPaneMessage -> List ViewPane -> Time.Posix -> ( Maybe ViewPane, PostUpdateAction )
update msg panes now =
    case msg of
        ChooseViewMode paneId mode ->
            let
                currentPane =
                    List.Extra.getAt paneId panes
            in
            case currentPane of
                Just pane ->
                    ( Just { pane | activeContext = mode }, ImageNoOp )

                Nothing ->
                    ( Nothing, ImageNoOp )

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
                            ( Just { pane | thirdPersonContext = newContext }, action )

                        ViewPlan ->
                            let
                                ( newContext, action ) =
                                    ScenePainterPlan.update imageMsg pane.planContext now
                            in
                            ( Just { pane | planContext = newContext }, action )

                        ViewProfile ->
                            let
                                ( newContext, action ) =
                                    ScenePainterProfile.update imageMsg pane.profileContext now
                            in
                            ( Just { pane | profileContext = newContext }, action )

                        _ ->
                            ( Just pane, ImageNoOp )

                Nothing ->
                    ( Nothing, ImageNoOp )

        AddPane ->
            -- Make visible first non-visible pane
            let
                firstHiddenPane =
                    List.Extra.find (not << .visible) panes

                paneMadeVisible =
                    Maybe.map (\pane -> { pane | visible = True }) firstHiddenPane
            in
            ( paneMadeVisible, ImageNoOp )

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
            ( paneHidden, ImageNoOp )

        EnlargePanes ->
            ( Nothing
            , PaneEnlarge
            )

        DiminishPanes ->
            ( Nothing
            , PaneDiminish
            )
