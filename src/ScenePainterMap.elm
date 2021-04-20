module ScenePainterMap exposing (..)

-- This is our map screen painter.
-- Works with Map Controller to talk to nasty JS stuff..

import Element exposing (..)
import Html.Attributes exposing (id)
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (..)
import TrackPoint exposing (TrackPoint)
import ViewingContext exposing (ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> List TrackPoint
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track
    in
    { defaultViewingContext
        | focalPoint = centralPoint
        , sceneSearcher = always Nothing
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewMap
    }


viewScene :
    ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene context scene wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size
    in
    el
        [ width <| px <| inPixels viewWidth
        , height <| px <| inPixels viewHeight
        , alignLeft
        , alignTop
        , htmlAttribute (id "map")
        ]
        (text "Ideally, there would be a map here.")

