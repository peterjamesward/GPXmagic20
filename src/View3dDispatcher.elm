module View3dDispatcher exposing (..)

import Element exposing (Element, none, text)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, PostUpdateAction(..), ViewingContext, ViewingMode(..))
import ScenePainterPlan
import ScenePainterThird
import Time


viewScene : Scene -> (ImageMsg -> msg) -> ViewingContext -> Element msg
viewScene scene wrapper context =
    let
        dispatch =
            case context.viewingMode of
                ThirdPerson ->
                    ScenePainterThird.viewScene

                Plan ->
                    ScenePainterPlan.viewScene

                _ ->
                    fallbackScenePainter
    in
    dispatch context scene wrapper


fallbackScenePainter _ _ _ =
    text "Any day now!"


update : ImageMsg -> ViewingContext -> Time.Posix -> ( ViewingContext, PostUpdateAction )
update msg context now =
    case context.viewingMode of
        ThirdPerson ->
            ScenePainterThird.update msg context now

        Plan ->
            ScenePainterPlan.update msg context now

        _ ->
            ( context, ImageNoOp )
