module LoopedTrack exposing (..)

import Direction3d
import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (UndoEntry)
import Quantity exposing (Quantity)
import Track exposing (Track)
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
                        <| case loopiness of
                            AlmostLoop _ ->
                                { onPress = Just <| wrap CloseTheLoop
                                , label = text "Make the track into a loop"
                                }

                            IsALoop ->
                                { onPress = Nothing
                                , label = text "Already a loop"
                                }

                            NotALoop _ ->
                                { onPress = Nothing
                                , label = text "Gap is too big"
                                }

                reverseButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap ReverseTrack
                        , label =
                            text <|
                                case isTrack.markedNode of
                                    Just _ ->
                                        "Reverse the track\nbetween the markers"

                                    Nothing ->
                                        "Reverse the track"
                        }

                changeStartButton c =
                    button
                        prettyButtonStyles
                        { onPress = Just (wrap <| ChangeLoopStart c)
                        , label = text "Move start/finish to current point"
                        }
            in
            wrappedRow [ spacing 10, padding 10 ] <|
                case loopiness of
                    IsALoop ->
                        [ text "This track is a loop."
                        , changeStartButton isTrack.currentNode
                        , reverseButton
                        ]

                    AlmostLoop gap ->
                        [ text <|
                            "This track is "
                                ++ showShortMeasure imperial gap
                                ++ " away from a loop"
                        , loopButton
                        , reverseButton
                        ]

                    NotALoop gap ->
                        [ text <|
                            "This track is "
                                ++ showDecimal0 (inMeters gap)
                                ++ "m\naway from a loop"
                        , loopButton
                        , reverseButton
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
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
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
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
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
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
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


applyReverse : ReverseUndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
applyReverse context track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt context.startOfReversedSection

        ( middle, suffix ) =
            theRest
                |> List.Extra.splitAt (context.endOfReversedSection - context.startOfReversedSection + 1)
    in
    ( prefix, List.reverse middle, suffix )


revertReverse : ReverseUndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
    ( prefix, List.reverse middle, suffix )


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


applyCloseLoop : ClosingInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
    ( [], outcome, [] )


revertCloseLoop : ClosingInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
    ( [], outcome, [] )


applyChangeLoopStart : Int -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
applyChangeLoopStart n track =
    let
        ( startToCurrent, currentToEnd ) =
            List.Extra.splitAt n track.trackPoints

        newStart =
            List.take 1 currentToEnd
    in
    ( [], currentToEnd ++ startToCurrent ++ newStart, [] )


revertChangeLoopStart : Int -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
revertChangeLoopStart n track =
    applyChangeLoopStart (List.length track.trackPoints - n - 1) track
