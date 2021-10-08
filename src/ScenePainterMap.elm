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
import Pixels exposing (Pixels, inPixels)
import PortController
import PostUpdateActions exposing (PostUpdateAction(..))
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
    -> ViewingContext
initialiseView viewSize track oldContext =
    -- This is just a simple default so we can see something!
    let
        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track.trackPoints
    in
    { oldContext
        | focalPoint = centralPoint
        , sceneSearcher = always Nothing
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewMap
    }


emptyPreviewCopy : Track -> Track
emptyPreviewCopy track =
    { track | trackPoints = [] }


initialiseMap : ViewingContext -> Track -> List (Cmd msg)
initialiseMap context track =
    [ PortController.addTrackToMap context track
    , PortController.addMarkersToMap track (emptyPreviewCopy track) (emptyPreviewCopy track)
    , PortController.centreMap context track
    , PortController.zoomMap context
    ]


mapTrackHasChanged : ViewingContext -> Track -> List (Cmd msg)
mapTrackHasChanged context track =
    [ PortController.addTrackToMap context track
    , PortController.addMarkersToMap track (emptyPreviewCopy track) (emptyPreviewCopy track)
    ]


update :
    ImageMsg
    -> ViewingContext
    -> (ImageMsg -> msg)
    -> ( ViewingContext, PostUpdateAction (Cmd msg) )
update msg view wrap =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageToggleClickToDragOnMap ->
            let
                newState =
                    not view.mapClickToDrag
            in
            ( { view | mapClickToDrag = newState }
            , ActionToggleMapDragging newState
            )

        _ ->
            ( view, ActionNoOp )


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

        handyMapControls =
            -- Might put the "click to drag" option here.
            column
                [ alignTop
                , alignRight
                , moveDown 100
                , moveLeft 10
                , Background.color white
                , Font.size 40
                , padding 6
                , spacing 8
                ]
                [ button []
                    { onPress = Just <| wrapper ImageToggleClickToDragOnMap
                    , label =
                        case context.mapClickToDrag of
                            True ->
                                useIcon <| FeatherIcons.move

                            False ->
                                useIcon <| FeatherIcons.xCircle
                    }
                ]
    in
    row [ spacing 0, padding 0, inFront handyMapControls ]
        [ el
            [ width <| px <| inPixels viewWidth
            , height <| px <| inPixels viewHeight
            , alignLeft
            , alignTop
            , htmlAttribute (id "map")
            ]
            none

        --, handyMapControls
        ]
