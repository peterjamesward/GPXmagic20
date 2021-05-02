module Loop exposing (..)

import Direction3d
import Element exposing (..)
import Element.Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import List.Extra
import Point3d
import PostUpdateActions
import Quantity exposing (Quantity)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (prettyButtonStyles)


info =
    """## Loop

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


viewLoopTools : Loopiness -> Maybe Track -> (Msg -> msg) -> Element msg
viewLoopTools loopiness track wrap =
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

                commonButtons =
                    wrappedRow [ spacing 10, padding 20, centerX ]
                        [ reverseButton ]
            in
            column [ spacing 10, padding 5, centerX ] <|
                case loopiness of
                    IsALoop ->
                        [ row [ spacing 10, padding 5, centerX ]
                            [ text "This track is a loop."
                            , changeStartButton isTrack.currentNode
                            ]
                        , commonButtons
                        ]

                    AlmostLoop gap ->
                        [ row [ spacing 10, padding 5, centerX ]
                            [ text <|
                                "This track is "
                                    ++ showDecimal0 (inMeters gap)
                                    ++ "\naway from a loop"
                            , loopButton
                            ]
                        , commonButtons
                        ]

                    NotALoop gap ->
                        [ row [ spacing 10, padding 5, centerX ]
                            [ text <|
                                "This track is "
                                    ++ showDecimal0 (inMeters gap)
                                    ++ " away from a loop"
                            , loopButton
                            ]
                        , commonButtons
                        ]


update :
    Msg
    -> Loopiness
    -> Track
    -> ( Loopiness, PostUpdateActions.PostUpdateAction msg)
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
                { track | track = List.reverse track.track }
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
            List.head track.track

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
                List.take (List.length track.track - 1) track.track
                    ++ List.take 1 track.track

            else
                -- A nicer solution here is to put a new trackpoint slightly "behind"
                -- the existing start, and then join the current last trackpoint to
                -- this new one. Existing tools can then be used to smooth as required.
                track.track
                    ++ List.map backOneMeter (List.take 1 track.track)
                    ++ List.take 1 track.track
    in
    case loopiness of
        AlmostLoop gap ->
            { track | track = newTrack gap }

        _ ->
            track


changeLoopStart : Track -> Track
changeLoopStart track =
    let
        n =
            track.currentNode.index

        ( startToCurrent, currentToEnd ) =
            List.Extra.splitAt n track.track

        newStart =
            List.take 1 currentToEnd
    in
    { track | track = currentToEnd ++ startToCurrent ++ newStart }
