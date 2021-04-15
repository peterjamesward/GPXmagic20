module ViewPane exposing (..)

import Axis3d exposing (Axis3d)
import ColourPalette exposing (radioButtonDefault, radioButtonSelected, radioButtonText)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import Length
import List.Extra
import LocalCoords exposing (LocalCoords)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, PostUpdateAction(..))
import ScenePainterPlan
import ScenePainterThird
import Time
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
    }


defaultViewPane =
    { paneId = 0
    , visible = True
    , activeContext = ViewThirdPerson
    , thirdPersonContext = newViewingContext ViewThirdPerson
    , firstPersonContext = newViewingContext ViewFirstPerson
    , planContext = newViewingContext ViewPlan
    , profileContext = newViewingContext ViewProfile
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


resetAllViews :
    List TrackPoint
    -> (Axis3d Length.Meters LocalCoords -> Maybe TrackPoint)
    -> ViewPane
    -> ViewPane
resetAllViews track searcher pane =
    { pane
        | thirdPersonContext = ScenePainterThird.initialiseView track searcher
        , firstPersonContext = ScenePainterThird.initialiseView track searcher
        , planContext = ScenePainterPlan.initialiseView track searcher
        , profileContext = ScenePainterPlan.initialiseView track searcher
    }


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


view : Scene -> (ViewPaneMessage -> msg) -> ViewPane -> Element msg
view scene wrapper pane =
    if pane.visible then
        column []
            [ row [ width fill ]
                [ el [ alignLeft ] <| viewModeChoices pane wrapper
                , viewAddAndRemove pane wrapper
                ]
            , viewScene
                scene
                (imageMessageWrapper pane.paneId >> wrapper)
                (getActiveContext pane)
            ]

    else
        none


viewScene : Scene -> (ImageMsg -> msg) -> ViewingContext -> Element msg
viewScene scene wrapper context =
    case context.viewingMode of
        ViewThirdPerson ->
            ScenePainterThird.viewScene context scene wrapper

        ViewPlan ->
            ScenePainterPlan.viewScene context scene wrapper

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
        [ button
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
