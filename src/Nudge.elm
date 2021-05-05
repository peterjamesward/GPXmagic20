module Nudge exposing (..)

import Angle
import Axis3d
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Graph exposing (applyIndexPreservingEditsToGraph)
import Length exposing (Length)
import List.Extra
import Point3d
import PostUpdateActions
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (..)


type NudgeMsg
    = SetHorizontalNudgeFactor Length
    | SetVerticalNudgeFactor Length
    | ZeroNudgeFactors
    | NudgeNode NudgeSettings


type NudgeEffects
    = NudgePreview (List TrackPoint)
    | NudgeTrackChanged String


type alias NudgeSettings =
    { horizontal : Length.Length
    , vertical : Length.Length
    , preview : List TrackPoint
    }


defaultNudgeSettings : NudgeSettings
defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , preview = []
    }


info =
    """## Nudge

You can adjust a single point or a section of track by
moving it up to 5m up or down, and left or right (relative
to direction of travel).

The view panes will show a preview as an orange line.
The change does not take effect until you click the
"Apply nudge" button"""


makeUndoMessage : Track -> String
makeUndoMessage track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( dist1, dist2 ) =
            ( Length.inMeters track.currentNode.distanceFromStart
            , Length.inMeters markerPosition.distanceFromStart
            )

        ( from, to ) =
            ( min dist1 dist2
            , max dist1 dist2
            )
    in
    if to > from then
        "Nudge from " ++ showDecimal0 from ++ " to " ++ showDecimal0 to

    else
        "Nudge node at" ++ showDecimal0 from


nudgeNodes : Track -> NudgeSettings -> Track
nudgeNodes track settings =
    -- Change the locations of the track points within the closed interval between
    -- markers, or just the current node if no purple cone.
    -- For a Graph, this must update canonical nodes and edges.
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        nudgedTrackPoints =
            nudgeNodeRange track.track from to settings

        newGraph =
            Maybe.map (applyIndexPreservingEditsToGraph ( from, to ) nudgedTrackPoints) track.graph

        newRoute =
            case newGraph of
                Just isGraph ->
                    Graph.walkTheRoute isGraph

                Nothing ->
                    nudgedTrackPoints

        newCurrent =
            List.Extra.getAt track.currentNode.index nudgedTrackPoints

        newMarker =
            case track.markedNode of
                Just isMarked ->
                    List.Extra.getAt isMarked.index nudgedTrackPoints

                Nothing ->
                    Nothing
    in
    { track
        | track = newRoute
        , graph = newGraph
        , currentNode = newCurrent |> Maybe.withDefault track.currentNode
        , markedNode = newMarker
    }


nudgeNodeRange : List TrackPoint -> Int -> Int -> NudgeSettings -> List TrackPoint
nudgeNodeRange trackPoints node1 nodeN settings =
    -- Apply the nudge factor permanently.
    let
        ( beforeLastPoint, afterLastPoint ) =
            List.Extra.splitAt (nodeN + 1) trackPoints

        ( beforeFirstPoint, targetTPs ) =
            List.Extra.splitAt node1 beforeLastPoint

        nudgedPoints =
            List.map
                (\tp -> nudgeTrackPoint tp settings)
                targetTPs

        newTrackPoints =
            beforeFirstPoint ++ nudgedPoints ++ afterLastPoint
    in
    newTrackPoints


nudgeTrackPoint : TrackPoint -> NudgeSettings -> TrackPoint
nudgeTrackPoint trackpoint settings =
    let
        horizontalDirection =
            trackpoint.effectiveDirection
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

        horizontalVector =
            Vector3d.withLength settings.horizontal horizontalDirection

        verticalVector =
            Vector3d.xyz Quantity.zero Quantity.zero settings.vertical

        newXYZ =
            trackpoint.xyz
                |> Point3d.translateBy horizontalVector
                |> Point3d.translateBy verticalVector

        newProfileXZ =
            trackpoint.profileXZ
                |> Point3d.translateBy verticalVector
    in
    { trackpoint | xyz = newXYZ, profileXZ = newProfileXZ }


previewNudgeNodes : NudgeSettings -> Track -> NudgeSettings
previewNudgeNodes settings track =
    -- Change the locations of the track points within the closed interval between
    -- markers, or just the current node if no purple cone.
    -- For a Graph, this must update canonical nodes and edges.
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        ( beforeLastPoint, afterLastPoint ) =
            List.Extra.splitAt (to + 1) track.track

        ( beforeFirstPoint, targetTPs ) =
            List.Extra.splitAt from beforeLastPoint

        nudgedPoints =
            List.map
                (\tp -> nudgeTrackPoint tp settings)
                targetTPs

        ( prevNode, postNode ) =
            ( List.Extra.getAt (from - 1) track.track
            , List.Extra.getAt (to + 1) track.track
            )

        nudgedListForVisuals =
            case ( prevNode, postNode ) of
                ( Just prev, Just post ) ->
                    [ prev ] ++ nudgedPoints ++ [ post ]

                ( Just prev, Nothing ) ->
                    [ prev ] ++ nudgedPoints

                ( Nothing, Just post ) ->
                    nudgedPoints ++ [ post ]

                ( Nothing, Nothing ) ->
                    nudgedPoints
    in
    { settings | preview = nudgedListForVisuals }


viewNudgeTools : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
viewNudgeTools settings msgWrapper =
    row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
        [ verticalNudgeSlider settings.vertical msgWrapper
        , column [ width fill, spaceEvenly, centerX, paddingXY 20 10, spacingXY 20 10 ]
            [ horizontalNudgeSlider settings.horizontal msgWrapper
            , nudgeButton settings msgWrapper
            , zeroButton msgWrapper
            ]
        ]


horizontalNudgeSlider : Length -> (NudgeMsg -> msg) -> Element msg
horizontalNudgeSlider value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetHorizontalNudgeFactor >> wrap
        , label =
            Input.labelBelow [ centerX ] <|
                text <|
                    (showDecimal2 <| Length.inMeters value)
        , min = -5.0
        , max = 5.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


verticalNudgeSlider : Length -> (NudgeMsg -> msg) -> Element msg
verticalNudgeSlider value wrap =
    el [ width <| px 80, centerX ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = Length.meters >> SetVerticalNudgeFactor >> wrap
            , label =
                Input.labelLeft [ centerY ] <|
                    text <|
                        showDecimal2 <|
                            Length.inMeters value
            , min = -5.0
            , max = 5.0
            , step = Nothing
            , value = Length.inMeters value
            , thumb = Input.defaultThumb
            }


nudgeButton : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
nudgeButton settings wrap =
    button
        prettyButtonStyles
        { onPress = Just <| wrap (NudgeNode settings)
        , label = text "Apply nudge"
        }


zeroButton : (NudgeMsg -> msg) -> Element msg
zeroButton wrap =
    button
        prettyButtonStyles
        { onPress = Just <| wrap ZeroNudgeFactors
        , label = text "Zero sliders"
        }


update :
    NudgeMsg
    -> NudgeSettings
    -> Track
    -> ( NudgeSettings, PostUpdateActions.PostUpdateAction msg )
update msg settings track =
    case msg of
        SetHorizontalNudgeFactor length ->
            let
                newSettings =
                    { settings | horizontal = length }
            in
            ( newSettings
            , PostUpdateActions.ActionPreview
            )

        SetVerticalNudgeFactor length ->
            let
                newSettings =
                    { settings | vertical = length }
            in
            ( newSettings
            , PostUpdateActions.ActionPreview
            )

        ZeroNudgeFactors ->
            ( defaultNudgeSettings
            , PostUpdateActions.ActionPreview
            )

        NudgeNode _ ->
            let
                nudgedPoints =
                    nudgeNodes track settings
            in
            ( { settings | preview = [] }
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (nudgeNodes track settings)
                (makeUndoMessage track)
            )


settingNotZero : NudgeSettings -> Bool
settingNotZero settings =
    Length.inMeters settings.horizontal
        /= 0.0
        || Length.inMeters settings.vertical
        /= 0.0
