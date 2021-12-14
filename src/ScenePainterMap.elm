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
import Json.Encode as E
import Length
import List.Extra
import LngLat exposing (LngLat)
import LocalCoords exposing (LocalCoords)
import MapBox
import MapCommands
import Mapbox.Style as Style
import MoveAndStretch
import Pixels exposing (Pixels, inPixels)
import Point3d
import PortController
import PostUpdateActions exposing (EditResult, PostUpdateAction(..), UndoEntry)
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import ScenePainterCommon exposing (..)
import Track exposing (Track)
import TrackEditType
import TrackPoint exposing (TrackPoint)
import TrackSearchQueries exposing (trackPointNearestFromIndexForPlan)
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
        | focalPoint = track.currentNode.xyz
        , sceneSearcher = trackPointNearestFromIndexForPlan track.spatialIndex
        , zoomLevel = zoom
        , defaultZoomLevel = zoom
        , viewingMode = ViewMap
        , mapClickToDrag = False
    }


emptyPreviewCopy : Track -> Track
emptyPreviewCopy track =
    { track | trackPoints = [] }


update :
    ImageMsg
    -> ViewingContext
    -> Track
    -> (ImageMsg -> msg)
    -> ( ViewingContext, PostUpdateAction trck (Cmd msg) )
update msg view track wrap =
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

        ImageZoomIn ->
            ( view, ActionCommand MapBox.zoomIn )

        ImageZoomOut ->
            ( view, ActionCommand MapBox.zoomOut )

        ImageReset ->
            ( view, ActionCommand <| MapBox.zoomReset view.defaultZoomLevel )

        MapClick { lngLat, renderedFeatures } ->
            case
                ( view.mapDrag
                , Track.searchTrackPointFromLonLat ( lngLat.lng, lngLat.lat ) track
                , view.mapClickToDrag
                )
            of
                ( Nothing, Just pointFound, True ) ->
                    -- First click starts drag
                    ( { view | mapDrag = Just pointFound }
                    , ActionNoOp
                    )

                ( Just drag, _, True ) ->
                    -- Second click is end of drag.
                    ( { view | mapDrag = Nothing }
                    , ActionTrackChanged
                        TrackEditType.EditPreservesIndex
                        (buildActions track drag)
                    )

                _ ->
                    ( view, ActionNoOp )

        MapMouseMove { lngLat, renderedFeatures } ->
            case view.mapDrag of
                Just drag ->
                    -- Update coordinates so we can show dot moving on Map.
                    let
                        newXYZwithoutAltitude =
                            TrackPoint.xyFromLngLat track.earthReferenceCoordinates ( lngLat.lng, lngLat.lat )

                        newXYZ =
                            Point3d.xyz
                                (Point3d.xCoordinate newXYZwithoutAltitude)
                                (Point3d.yCoordinate newXYZwithoutAltitude)
                                (Point3d.zCoordinate drag.xyz)

                        draggedPoint =
                            { drag | xyz = newXYZ }
                    in
                    ( { view | mapDrag = Just draggedPoint }
                    , ActionPreview
                    )

                Nothing ->
                    ( view, ActionNoOp )

        MapMouseUp { lngLat, renderedFeatures } ->
            case view.mapDrag of
                Just drag ->
                    let
                        dragged =
                            { drag
                                | xyz =
                                    TrackPoint.xyFromLngLat track.earthReferenceCoordinates ( lngLat.lng, lngLat.lat )
                            }
                    in
                    ( { view | mapDrag = Just dragged }
                    , ActionPreview
                    )

                Nothing ->
                    ( view, ActionNoOp )

        _ ->
            ( view, ActionNoOp )


viewScene :
    ViewingContext
    -> Style.Style
    -> (ImageMsg -> msg)
    -> Element msg
viewScene context style wrapper =
    let
        ( viewWidth, viewHeight ) =
            context.size

        handyMapControls =
            column
                [ alignTop
                , alignRight
                , moveDown 90
                , moveLeft 5
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
    el
        [ spacing 0
        , padding 0
        , inFront handyMapControls
        , inFront <| zoomButtons wrapper
        ]
    <|
        el
            [ width <| px <| inPixels viewWidth
            , height <| px <| inPixels viewHeight
            , alignLeft
            , alignTop
            , htmlAttribute (id "map")
            ]
        <|
            html <|
                MapBox.view context style wrapper


type alias UndoRedoInfo =
    { index : Int
    , point : TrackPoint
    , originalXYZ : Point3d.Point3d Length.Meters LocalCoords
    , adjustedXYZ : Point3d.Point3d Length.Meters LocalCoords
    }


buildActions : Track -> TrackPoint -> UndoEntry
buildActions track point =
    -- This is the +/-ve delta for possible redo. We do not include track in the closure!
    let
        undoRedo =
            { index = point.index
            , point = point
            , originalXYZ =
                --TODO: Store original location on drag start to avoid this lookup.
                track.trackPoints
                    |> List.Extra.getAt point.index
                    |> Maybe.withDefault track.currentNode
                    |> .xyz
            , adjustedXYZ = point.xyz
            }
    in
    { label = "Drag on map"
    , editFunction = editFunction undoRedo
    , undoFunction = undoFunction undoRedo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


editFunction : UndoRedoInfo -> Track -> EditResult
editFunction undoRedoInfo track =
    let
        focalPoint =
            undoRedoInfo.point
    in
    { before = track.trackPoints |> List.take undoRedoInfo.index
    , edited = [ { focalPoint | xyz = undoRedoInfo.adjustedXYZ } ]
    , after = track.trackPoints |> List.drop (undoRedoInfo.index + 1)
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


undoFunction : UndoRedoInfo -> Track -> EditResult
undoFunction undoRedoInfo track =
    let
        focalPoint =
            undoRedoInfo.point
    in
    { before = track.trackPoints |> List.take undoRedoInfo.index
    , edited = [ { focalPoint | xyz = undoRedoInfo.originalXYZ } ]
    , after = track.trackPoints |> List.drop (undoRedoInfo.index + 1)
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }
