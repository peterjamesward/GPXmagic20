module ScenePainterMap exposing (..)

-- This is our map screen painter.
-- Works with Map Controller to talk to nasty JS stuff..

import Element exposing (..)
import Html.Attributes exposing (id)
import MapController
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import SceneBuilder exposing (Scene)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import ViewPureStyles exposing (conditionallyVisible)
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


initialiseMap : ViewingContext -> Track -> List (Cmd msg)
initialiseMap context track =
    [ MapController.addTrackToMap context track
    , MapController.centreMap context track
    ]


viewScene :
    Bool
    -> ViewingContext
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context scene wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size
    in
    row [ spacing 0, padding 0 ]
        [ el
            [ width <| px <| inPixels viewWidth
            , height <| px <| inPixels viewHeight
            , alignLeft
            , alignTop
            , htmlAttribute (id "map")
            ]
            none
        , conditionallyVisible False <| zoomButtons wrapper
        ]
