module DeletePoints exposing (..)

import Element exposing (Element, column, text)
import Element.Input exposing (button)
import Graph exposing (applyNodePreservingEditsToGraph)
import Length
import List.Extra
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (defaultColumnLayout, prettyButtonStyles)


type Msg
    = DeleteTrackPoints


type Action
    = DeleteTrackChanged String
    | DeleteNoOp


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
        "Delete point at " ++ showDecimal2 start

    else
        "Delete from " ++ showDecimal2 start ++ " to " ++ showDecimal2 finish


update : Msg -> Track -> ( Track, Action )
update msg track =
    case msg of
        DeleteTrackPoints ->
            ( deletePoints track, DeleteTrackChanged <| makeUndoMessage track )


deletePoints : Track -> Track
deletePoints track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( start, finish ) =
            ( min track.currentNode.index marker.index
            , max track.currentNode.index marker.index
            )

        remainingTrackPoints =
            track.track
                |> List.Extra.removeIfIndex (\i -> i >= start && i <= finish)

        newGraph =
            Maybe.map
                (applyNodePreservingEditsToGraph ( start, finish ) remainingTrackPoints)
                track.graph

        newRoute =
            case newGraph of
                Just isGraph ->
                    Graph.walkTheRoute isGraph

                Nothing ->
                    remainingTrackPoints

        newCurrent =
            List.Extra.getAt track.currentNode.index remainingTrackPoints

        newMarker =
            Nothing
    in
    { track
        | track = newRoute
        , graph = newGraph
        , currentNode = newCurrent |> Maybe.withDefault track.currentNode
        , markedNode = newMarker
    }
