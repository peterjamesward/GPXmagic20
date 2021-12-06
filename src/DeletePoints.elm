module DeletePoints exposing (..)

import Color
import Element exposing (Element, column, text)
import Element.Input exposing (button)
import Graph exposing (applyNodePreservingEditsToGraph)
import List.Extra
import LocalCoords exposing (LocalCoords)
import PostUpdateActions exposing (PostUpdateAction, UndoEntry)
import Quantity
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showLongMeasure)
import ViewPureStyles exposing (defaultColumnLayout, prettyButtonStyles)


type Msg
    = DeleteTrackPoints


type Action
    = DeleteTrackChanged String
    | DeleteNoOp


type alias UndoRedoInfo =
    -- We should be able to Undo by working out the new points afresh.
    -- The options will also be available in the closure.
    { start : Int
    , end : Int
    , originalPoints : List TrackPoint
    }


toolLabel =
    "Delete"


info =
    """## Delete track points

Without the purple marker, the "Delete" button will delete the single track point
at the orange marker.

With the purple marker, it will delete the track points
bwteeen and including the two markers.

A straight section of track will replace the deleted section.

It is not possible to delete a Graph Node.
"""


viewDeleteTools : Bool -> Maybe Track -> (Msg -> msg) -> Element msg
viewDeleteTools imperial track msgWrapper =
    case track of
        Nothing ->
            text "Hmm, we have no track."

        Just isTrack ->
            let
                deleteNodeButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| msgWrapper DeleteTrackPoints
                        , label = text <| makeUndoMessage imperial isTrack
                        }
            in
            column defaultColumnLayout
                [ deleteNodeButton
                ]


makeUndoMessage : Bool -> Track -> String
makeUndoMessage imperial track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( start, finish ) =
            ( Quantity.min track.currentNode.distanceFromStart marker.distanceFromStart
            , Quantity.max track.currentNode.distanceFromStart marker.distanceFromStart
            )
    in
    if start == finish then
        "Delete point at " ++ showLongMeasure imperial start

    else
        "Delete from " ++ showLongMeasure imperial start ++ " to " ++ showLongMeasure imperial finish


update : Bool -> Msg -> Track -> PostUpdateAction trck msg
update imperial msg track =
    case msg of
        DeleteTrackPoints ->
            PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                (buildActions imperial track)


buildActions : Bool -> Track -> UndoEntry
buildActions imperial track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.currentNode.index <= marker.index then
                ( track.currentNode, marker )

            else
                ( marker, track.currentNode )

        safeStart =
            -- Avoid deletion of whole track.
            if startPoint.index == 0 && endPoint.index == List.length track.trackPoints - 1 then
                1

            else
                startPoint.index

        undoMessage =
            makeUndoMessage imperial track

        originalPoints =
            track.trackPoints |> List.take (1 + endPoint.index) |> List.drop safeStart

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { start = safeStart
            , end = endPoint.index
            , originalPoints = originalPoints
            }

        newOrange =
            if track.currentNode.index == endPoint.index then
                endPoint.index - List.length originalPoints

            else
                track.currentNode.index

        newPurple =
            if track.markedNode == Just endPoint then
                Just (endPoint.index - List.length originalPoints)

            else
                Maybe.map .index track.markedNode
    in
    { label = undoMessage
    , editFunction = apply undoRedoInfo
    , undoFunction = undo undoRedoInfo
    , newOrange = newOrange
    , newPurple = newPurple
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


apply : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
apply undoRedo track =
    let
        pointsToDelete =
            undoRedo.end - undoRedo.start + 1

        ( prefix, theRest ) =
            track.trackPoints |> List.Extra.splitAt undoRedo.start

        ( toDelete, suffix ) =
            theRest |> List.Extra.splitAt pointsToDelete
    in
    ( prefix, [], suffix )


undo : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
undo undoRedo track =
    let
        ( prefix, suffix ) =
            track.trackPoints |> List.Extra.splitAt undoRedo.start
    in
    ( prefix, undoRedo.originalPoints, suffix )


getPreview3D : Track -> List (Entity LocalCoords)
getPreview3D track =
    let
        actions =
            buildActions False track

        ( _, toDelete, _ ) =
            actions.undoFunction track
    in
    highlightPoints Color.lightRed toDelete
