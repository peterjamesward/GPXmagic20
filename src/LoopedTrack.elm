module LoopedTrack exposing (..)

import Direction3d
import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import List.Extra
import Point3d
import PostUpdateActions
import Quantity exposing (Quantity)
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2, showShortMeasure)
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)


info =
    """## LoopedTrack

If your route is nearly a loop, but no close enough for RGT
to recognise it, you can use this to add a final road section
to return to the start. It will *not* make it smooth; that's up
to you.

Once you have a loop, you can reverse the direction of travel,
or you can change the start point. This will not be the RGT Start/Finish
location which is (famously) about 60m along the road.

You may want to temporarily move the start so you can use the other
tools across the start/finish, then move the start point back.
"""


type
    Loopiness
    -- Not sure this is the best place, but better here than Main.
    = NotALoop (Quantity Float Meters)
    | IsALoop
    | AlmostLoop (Quantity Float Meters) -- if, say, less than 200m back to start.


type Msg
    = CloseTheLoop
    | ReverseTrack
    | ChangeLoopStart TrackPoint


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
                        { onPress = Just <| wrap CloseTheLoop
                        , label = text "Make the track into a loop"
                        }

                reverseButton =
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap ReverseTrack
                        , label = text "Reverse the track"
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
    -> ( Loopiness, PostUpdateActions.PostUpdateAction msg )
update msg settings track =
    case msg of
        CloseTheLoop ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                (closeTheLoop track settings)
                "close the loop"
            )

        ReverseTrack ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                { track | trackPoints = List.reverse track.trackPoints }
                "reverse track"
            )

        ChangeLoopStart tp ->
            let
                newSettings =
                    settings
            in
            ( newSettings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                (changeLoopStart track)
                ("move start to "
                    ++ (showDecimal2 <| inMeters track.currentNode.distanceFromStart)
                )
            )


closeTheLoop : Track -> Loopiness -> Track
closeTheLoop track loopiness =
    let
        maybeFirstPoint =
            List.head track.trackPoints

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

        newTrack gap =
            if gap |> Quantity.lessThanOrEqualTo (meters 1.0) then
                -- Replace last trackpoint with the first as we are so close.
                List.take (List.length track.trackPoints - 1) track.trackPoints
                    ++ List.take 1 track.trackPoints

            else
                -- A nicer solution here is to put a new trackpoint slightly "behind"
                -- the existing start, and then join the current last trackpoint to
                -- this new one. Existing tools can then be used to smooth as required.
                track.trackPoints
                    ++ List.map backOneMeter (List.take 1 track.trackPoints)
                    ++ List.take 1 track.trackPoints
    in
    case loopiness of
        AlmostLoop gap ->
            { track | trackPoints = newTrack gap }

        _ ->
            track


changeLoopStart : Track -> Track
changeLoopStart track =
    let
        n =
            track.currentNode.index

        ( startToCurrent, currentToEnd ) =
            List.Extra.splitAt n track.trackPoints

        newStart =
            List.take 1 currentToEnd
    in
    { track | trackPoints = currentToEnd ++ startToCurrent ++ newStart }
