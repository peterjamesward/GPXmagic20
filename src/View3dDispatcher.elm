module View3dDispatcher exposing (..)

import Element exposing (Element, none, text)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (ImageMsg, ViewingContext, ViewingMode(..))
import ScenePainterThird


viewScene : ViewingContext -> Scene -> (ImageMsg -> msg) -> Element msg
viewScene context scene wrapper =
    let
        dispatch =
            case context.viewingMode of
                ThirdPerson ->
                    ScenePainterThird.viewScene

                _ ->
                    fallbackScenePainter
    in
    dispatch context scene wrapper


fallbackScenePainter _ _ _ =
    text "Any day now!"
