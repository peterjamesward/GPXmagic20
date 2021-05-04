module ScenePainterMap exposing (..)

-- This is our map screen painter.
-- Works with Map Controller to talk to nasty JS stuff..

import ColourPalette exposing (white)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html.Attributes exposing (id)
import MapController
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import Utils exposing (useIcon)
import ViewingContext exposing (ViewingContext, defaultViewingContext)
import ViewingMode exposing (ViewingMode(..))


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Track
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track.track
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
    , MapController.addMarkersToMap track [] []
    , MapController.centreMap context track
    , MapController.zoomMap context
    ]


mapTrackHasChanged : ViewingContext -> Track -> List (Cmd msg)
mapTrackHasChanged context track =
    [ MapController.addTrackToMap context track
    , MapController.addMarkersToMap track [] []
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
        , handyMapControls wrapper
        ]


handyMapControls wrap =
    -- Might put the "click to drag" option here.
    column
        [ alignTop
        , moveDown 5
        , moveLeft 40
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        ]
        [ button []
            { onPress = Just <| wrap ImageNoOpMsg
            , label = useIcon FeatherIcons.move
            }
        ]
