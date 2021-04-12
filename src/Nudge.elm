module Nudge exposing (..)

import Angle
import Axis3d
import Direction3d
import Element exposing (Element, centerX, column, el, px, row, text, width)
import Element.Input as Input exposing (button)
import Graph exposing (applyIndexPreservingEditsToGraph)
import Length exposing (Length)
import List.Extra
import Point3d
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal2)
import Vector3d
import ViewPureStyles exposing (..)


type NudgeMsg
    = SetHorizontalNudgeFactor Length
    | SetVerticalNudgeFactor Length
    | ZeroNudgeFactors
    | NudgeNode NudgeSettings


type NudgeEffects
    = NudgeControlsOnly
    | NudgeTrackChanged String


type alias NudgeSettings =
    { horizontal : Length
    , vertical : Length
    }


defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    }


makeUndoMessage : Track -> String
makeUndoMessage track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )
    in
    if to > from then
        "Nudge " ++ String.fromInt from ++ " to " ++ String.fromInt to

    else
        "Nudge node " ++ String.fromInt from


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
    in
    { trackpoint | xyz = newXYZ }


viewNudgeTools : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
viewNudgeTools settings msgWrapper =
    column defaultColumnLayout
        [ row defaultRowLayout
            [ verticalNudgeSlider settings.vertical msgWrapper
            , column defaultColumnLayout
                [ horizontalNudgeSlider settings.horizontal msgWrapper
                , nudgeButton settings msgWrapper
                , zeroButton msgWrapper
                ]
            ]
        ]


horizontalNudgeSlider : Length -> (NudgeMsg -> msg) -> Element msg
horizontalNudgeSlider value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetHorizontalNudgeFactor >> wrap
        , label =
            Input.labelBelow [] <|
                text <|
                    "Offset = "
                        ++ (showDecimal2 <| Length.inMeters value)
                        ++ "m"
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
                Input.labelBelow [ centerX ] <|
                    text <|
                        "Height = "
                            ++ (showDecimal2 <| Length.inMeters value)
                            ++ "m"
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


update : NudgeMsg -> NudgeSettings -> Track -> ( NudgeSettings, Track, NudgeEffects )
update msg settings track =
    case msg of
        SetHorizontalNudgeFactor length ->
            ( { settings | horizontal = length }, track, NudgeControlsOnly )

        SetVerticalNudgeFactor length ->
            ( { settings | vertical = length }, track, NudgeControlsOnly )

        ZeroNudgeFactors ->
            ( defaultNudgeSettings, track, NudgeControlsOnly )

        NudgeNode _ ->
            let
                undoMessage =
                    makeUndoMessage track
            in
            ( settings, nudgeNodes track settings, NudgeTrackChanged undoMessage )
