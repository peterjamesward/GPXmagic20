module LoopedTrack exposing (..)

import Dict
import Direction3d
import Element exposing (..)
import Element.Input exposing (button)
import Graph exposing (Graph)
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (EditResult, UndoEntry)
import Quantity exposing (Quantity)
import Track exposing (Track)
import TrackEditType
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showShortMeasure)
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)


toolLabel =
    "Looped Track maker"


info =
    """## Looped Track

If your route is nearly a loop, but not close enough for RGT
to recognise it, you can use this to add a final road section
to return to the start. It will *not* make it smooth; that's up
to you.

Once you have a loop, you can reverse the direction of travel,
or you can change the start point. This will not be the RGT Start/Finish
location which is about 60m along the road.

You may want to temporarily move the start so you can use the other
tools across the start/finish, then move the start point back.
"""


type Loopiness
    = NotALoop (Quantity Float Meters)
    | IsALoop
    | AlmostLoop (Quantity Float Meters) -- if, say, less than 200m back to start.


type Msg
    = CloseTheLoop
    | ReverseTrack
    | ChangeLoopStart TrackPoint
    | OutAndBack Length.Length


type alias ReverseUndoRedoInfo =
    { startOfReversedSection : Int
    , endOfReversedSection : Int
    , orange : Int
    , purple : Maybe Int
    }


viewLoopTools : Bool -> Loopiness -> Maybe Track -> (Msg -> msg) -> Element msg
viewLoopTools imperial loopiness track wrap =
    case track of
        Nothing ->
            none

        Just isTrack ->
            let
                loopButton =
                    button
                        prettyButtonStyles
                    <|
                        case loopiness of
                            AlmostLoop _ ->
                                { onPress = Just <| wrap CloseTheLoop
                                , label = paragraph [] [ text "Make the track into a loop" ]
                                }

                            IsALoop ->
                                { onPress = Nothing
                                , label = paragraph [] [ text "Already a loop" ]
                                }

                            NotALoop _ ->
                                { onPress = Nothing
                                , label = paragraph [] [ text "Gap is too big" ]
                                }

                reverseButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap ReverseTrack
                        , label =
                            paragraph []
                                [ text <|
                                    case isTrack.markedNode of
                                        Just _ ->
                                            "Reverse the track\nbetween the markers"

                                        Nothing ->
                                            "Reverse the track"
                                ]
                        }

                changeStartButton c =
                    button
                        prettyButtonStyles
                        { onPress = Just (wrap <| ChangeLoopStart c)
                        , label = paragraph [] [ text "Move start/finish to current point" ]
                        }

                outAndBackLeft =
                    button
                        prettyButtonStyles
                        { onPress = Just (wrap <| OutAndBack (Length.meters -5))
                        , label =
                            paragraph [] <|
                                [ text "Create a looped out and back course, offset 5m left of centre" ]
                        }

                outAndBackRight =
                    button
                        prettyButtonStyles
                        { onPress = Just (wrap <| OutAndBack (Length.meters 5))
                        , label =
                            paragraph [] <|
                                [ text "Create a looped out and back course, offset 5m right of centre" ]
                        }
            in
            column [ spacing 10, padding 10, width fill ] <|
                case loopiness of
                    IsALoop ->
                        [ paragraph [] [ text "This track is a loop." ]
                        , changeStartButton isTrack.currentNode
                        , reverseButton
                        ]

                    AlmostLoop gap ->
                        [ paragraph []
                            [ text <|
                                "This track is "
                                    ++ showShortMeasure imperial gap
                                    ++ " away from a loop"
                            ]
                        , loopButton
                        , reverseButton
                        , outAndBackLeft
                        , outAndBackRight
                        ]

                    NotALoop gap ->
                        [ paragraph []
                            [ text <|
                                "This track is "
                                    ++ showDecimal0 (inMeters gap)
                                    ++ "m\naway from a loop"
                            ]
                        , loopButton
                        , reverseButton
                        , outAndBackLeft
                        , outAndBackRight
                        ]


update :
    Msg
    -> Loopiness
    -> Track
    -> ( Loopiness, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        CloseTheLoop ->
            let
                undoRedoInfo =
                    saveContextForClosingLoop track settings

                actionEntry : UndoEntry
                actionEntry =
                    { label = "Close the loop"
                    , editFunction = applyCloseLoop undoRedoInfo
                    , undoFunction = revertCloseLoop undoRedoInfo
                    , newOrange = track.currentNode.index
                    , newPurple = Maybe.map .index track.markedNode
                    , oldOrange = track.currentNode.index
                    , oldPurple = Maybe.map .index track.markedNode
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesNodePosition
                actionEntry
            )

        ReverseTrack ->
            let
                undoRedoInfo =
                    saveContextForReverse track

                actionEntry : UndoEntry
                actionEntry =
                    { label = "Reverse track"
                    , editFunction = applyReverse undoRedoInfo
                    , undoFunction = revertReverse undoRedoInfo
                    , newOrange = undoRedoInfo.orange
                    , newPurple = undoRedoInfo.purple
                    , oldOrange = track.currentNode.index
                    , oldPurple = Maybe.map .index track.markedNode
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesNodePosition
                actionEntry
            )

        ChangeLoopStart tp ->
            let
                undoRedoInfo =
                    track.currentNode.index

                actionEntry : UndoEntry
                actionEntry =
                    { label = "Move start to " ++ String.fromInt track.currentNode.index
                    , editFunction = applyChangeLoopStart undoRedoInfo
                    , undoFunction = revertChangeLoopStart undoRedoInfo
                    , newOrange = 0
                    , newPurple = Maybe.map .index track.markedNode
                    , oldOrange = track.currentNode.index
                    , oldPurple = Maybe.map .index track.markedNode
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesNodePosition
                actionEntry
            )

        OutAndBack offsetSide ->
            let
                undoRedoInfo =
                    ( List.length track.trackPoints, offsetSide, track.currentNode )

                actionEntry : UndoEntry
                actionEntry =
                    { label = "Convert to out and back"
                    , editFunction = applyOutAndBack undoRedoInfo
                    , undoFunction = revertOutAndBack undoRedoInfo
                    , newOrange = track.currentNode.index
                    , newPurple = Maybe.map .index track.markedNode
                    , oldOrange = track.currentNode.index
                    , oldPurple = Maybe.map .index track.markedNode
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesNodePosition
                actionEntry
            )


saveContextForReverse : Track -> ReverseUndoRedoInfo
saveContextForReverse track =
    case track.markedNode of
        Just marker ->
            let
                ( current, marked ) =
                    ( track.currentNode.index, marker.index )

                ( start, end ) =
                    ( min current marked, max current marked )

                undoRedoInfo =
                    { startOfReversedSection = start
                    , endOfReversedSection = end
                    , orange = List.length track.trackPoints - track.currentNode.index - 1
                    , purple =
                        case track.markedNode of
                            Just purple ->
                                Just <| List.length track.trackPoints - purple.index - 1

                            Nothing ->
                                Nothing
                    }
            in
            undoRedoInfo

        Nothing ->
            { startOfReversedSection = 0
            , endOfReversedSection = List.length track.trackPoints - 1
            , orange = List.length track.trackPoints - track.currentNode.index - 1
            , purple = Nothing
            }


applyReverse : ReverseUndoRedoInfo -> Track -> EditResult
applyReverse context track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt context.startOfReversedSection

        ( middle, suffix ) =
            theRest
                |> List.Extra.splitAt (context.endOfReversedSection - context.startOfReversedSection + 1)
    in
    { before = prefix
    , edited = List.reverse middle
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


revertReverse : ReverseUndoRedoInfo -> Track -> EditResult
revertReverse context track =
    let
        ( startInReverse, endInReverse ) =
            ( List.length track.trackPoints - context.endOfReversedSection - 1
            , List.length track.trackPoints - context.startOfReversedSection - 1
            )

        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt startInReverse

        ( middle, suffix ) =
            theRest
                |> List.Extra.splitAt (endInReverse - startInReverse + 1)
    in
    { before = prefix
    , edited = List.reverse middle
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


type alias ClosingInfo =
    { originalEnd : Point3d.Point3d Length.Meters LocalCoords
    , pointWasAdded : Bool
    , willClose : Bool
    }


saveContextForClosingLoop : Track -> Loopiness -> ClosingInfo
saveContextForClosingLoop track loopiness =
    let
        originalEndXYZ : Point3d.Point3d Length.Meters LocalCoords
        originalEndXYZ =
            track.trackPoints
                |> List.Extra.last
                |> Maybe.map .xyz
                |> Maybe.withDefault track.currentNode.xyz
    in
    case loopiness of
        AlmostLoop gap ->
            if gap |> Quantity.lessThanOrEqualTo (meters 1.0) then
                { originalEnd = originalEndXYZ
                , pointWasAdded = False
                , willClose = True
                }

            else
                -- A nicer solution here is to put a new trackpoint slightly "behind"
                -- the existing start, and then join the current last trackpoint to
                -- this new one. Existing tools can then be used to smooth as required.
                { originalEnd = originalEndXYZ
                , pointWasAdded = True
                , willClose = True
                }

        _ ->
            { originalEnd = originalEndXYZ
            , pointWasAdded = False
            , willClose = False
            }


applyCloseLoop : ClosingInfo -> Track -> EditResult
applyCloseLoop editInfo track =
    let
        backOneMeter : TrackPoint -> TrackPoint
        backOneMeter startPoint =
            let
                trackDirection =
                    startPoint.afterDirection
                        |> Maybe.withDefault Direction3d.positiveX
                        |> Direction3d.reverse

                shiftVector =
                    trackDirection |> Vector3d.withLength (meters 1.0)

                newLocation =
                    Point3d.translateBy
                        shiftVector
                        startPoint.xyz
            in
            { startPoint | xyz = newLocation }

        outcome =
            case ( editInfo.willClose, editInfo.pointWasAdded ) of
                ( True, False ) ->
                    -- Replace last trackpoint with the first as we are so close.
                    List.take (List.length track.trackPoints - 1) track.trackPoints
                        ++ List.take 1 track.trackPoints

                ( True, True ) ->
                    -- A nicer solution here is to put a new trackpoint slightly "behind"
                    -- the existing start, and then join the current last trackpoint to
                    -- this new one. Existing tools can then be used to smooth as required.
                    track.trackPoints
                        ++ List.map backOneMeter (List.take 1 track.trackPoints)
                        ++ List.take 1 track.trackPoints

                ( False, _ ) ->
                    track.trackPoints
    in
    { before = []
    , edited = outcome
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


revertCloseLoop : ClosingInfo -> Track -> EditResult
revertCloseLoop editInfo track =
    let
        outcome =
            case ( editInfo.willClose, editInfo.pointWasAdded ) of
                ( True, False ) ->
                    -- Replace last trackpoint with the first as we are so close.
                    List.take (List.length track.trackPoints - 1) track.trackPoints

                ( True, True ) ->
                    -- A nicer solution here is to put a new trackpoint slightly "behind"
                    -- the existing start, and then join the current last trackpoint to
                    -- this new one. Existing tools can then be used to smooth as required.
                    List.take (List.length track.trackPoints - 2) track.trackPoints

                ( False, _ ) ->
                    track.trackPoints
    in
    { before = []
    , edited = outcome
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


applyChangeLoopStart : Int -> Track -> EditResult
applyChangeLoopStart n track =
    let
        ( startToCurrent, currentToEnd ) =
            List.Extra.splitAt n track.trackPoints

        newStart =
            List.take 1 currentToEnd
    in
    { before = []
    , edited = currentToEnd ++ startToCurrent ++ newStart
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


revertChangeLoopStart : Int -> Track -> EditResult
revertChangeLoopStart n track =
    applyChangeLoopStart (List.length track.trackPoints - n - 1) track


applyOutAndBack : ( Int, Length.Length, TrackPoint ) -> Track -> EditResult
applyOutAndBack ( originalLength, offset, current ) track =
    --TODO: (functionality to go in Graph module?)
    -- 1. Convert to graph
    -- 2. Add return leg and second forward leg (for turn-around)
    -- 3. Convert back with offset
    -- 4. Prune the second outward leg (or dont add it somehow)
    let
        loopedTrack =
            Graph.makeOutAndBack offset track.trackPoints

        prunedOutAndBack =
            loopedTrack |> List.take (1 + List.length loopedTrack - originalLength)
    in
    { before = []
    , edited = prunedOutAndBack
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


revertOutAndBack : ( Int, Length.Length, TrackPoint ) -> Track -> EditResult
revertOutAndBack ( originalLength, _, _ ) track =
    { before = []
    , edited = List.take originalLength track.trackPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = Nothing
    }
