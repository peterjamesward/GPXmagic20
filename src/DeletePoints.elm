module DeletePoints exposing (..)

import Element exposing (Element, column, text)
import Element.Input exposing (button)
import Graph exposing (applyNodePreservingEditsToGraph)
import Length
import List.Extra
import PostUpdateActions exposing (PostUpdateAction)
import Quantity
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (defaultColumnLayout, prettyButtonStyles)


type Msg
    = DeleteTrackPoints


type Action
    = DeleteTrackChanged String
    | DeleteNoOp


info =
    """## Delete track points

Without the purple marker, the "Delete" button will delete the single track point
at the orange marker.

With the purple marker, it will delete the track points
bwteeen and including the two markers.

A straight section of track will replace the deleted section.

It is not possible to delete a Graph Node.
"""


viewDeleteTools : Maybe Track -> (Msg -> msg) -> Element msg
viewDeleteTools track msgWrapper =
    case track of
        Nothing ->
            text "Hmm, we have no track."

        Just isTrack ->
            let
                deleteNodeButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| msgWrapper DeleteTrackPoints
                        , label = text <| makeUndoMessage isTrack
                        }
            in
            column defaultColumnLayout
                [ deleteNodeButton
                ]


makeUndoMessage : Track -> String
makeUndoMessage track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( start, finish ) =
            ( Length.inMeters <|
                Quantity.min track.currentNode.distanceFromStart marker.distanceFromStart
            , Length.inMeters <|
                Quantity.max track.currentNode.distanceFromStart marker.distanceFromStart
            )
    in
    if start == finish then
        "Delete point at " ++ showDecimal0 start

    else
        "Delete from " ++ showDecimal0 start ++ " to " ++ showDecimal0 finish


update : Msg -> Track -> PostUpdateAction msg
update msg track =
    case msg of
        DeleteTrackPoints ->
            PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                (deletePoints track)
                (makeUndoMessage track)


deletePoints : Track -> Track
deletePoints track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( start, finish ) =
            ( min track.currentNode.index marker.index
            , max track.currentNode.index marker.index
            )

        safeStart =
            if start == 0 && finish == List.length track.trackPoints - 1 then
                1
            else
                start

        pointsToDelete =
            finish - safeStart + 1

        newRoute =
            track.trackPoints
                |> List.Extra.removeIfIndex (\i -> i >= safeStart && i <= finish)
                |> TrackPoint.prepareTrackPoints

        newCurrent =
            if start == track.currentNode.index then
                List.Extra.getAt
                    (max 0 (track.currentNode.index - 1))
                    newRoute

            else
                List.Extra.getAt
                    (min (finish - pointsToDelete) (List.length track.trackPoints - 1))
                    newRoute
    in
    { track
        | trackPoints = newRoute
        , currentNode = newCurrent |> Maybe.withDefault track.currentNode
        , markedNode = Nothing
    }
