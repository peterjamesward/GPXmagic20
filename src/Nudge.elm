module Nudge exposing (..)

import Angle
import Axis3d
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Length, inMeters)
import List.Extra
import Point3d
import PostUpdateActions
import Quantity
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (..)


type NudgeMsg
    = SetHorizontalNudgeFactor Length.Length
    | SetVerticalNudgeFactor Length.Length
    | ZeroNudgeFactors
    | NudgeNode NudgeSettings
    | SetFadeExtent Length.Length


type NudgeEffects
    = NudgePreview (List TrackPoint)
    | NudgeTrackChanged String


type alias NudgeSettings =
    { horizontal : Length.Length
    , vertical : Length.Length
    , preview : List TrackPoint
    , fadeExtent : Length.Length
    }


defaultNudgeSettings : NudgeSettings
defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , fadeExtent = Quantity.zero
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
            computeNudgedPoints settings track

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
        | trackPoints = nudgedTrackPoints
        , currentNode = newCurrent |> Maybe.withDefault track.currentNode
        , markedNode = newMarker
    }


nudgeTrackPoint : NudgeSettings -> Float -> TrackPoint -> TrackPoint
nudgeTrackPoint settings fade trackpoint =
    if fade == 0 then
        trackpoint

    else
        let
            horizontalDirection =
                trackpoint.effectiveDirection
                    |> Maybe.withDefault Direction3d.x
                    |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

            horizontalVector =
                Vector3d.withLength settings.horizontal horizontalDirection
                    |> Vector3d.scaleBy fade

            verticalVector =
                Vector3d.xyz Quantity.zero Quantity.zero settings.vertical
                    |> Vector3d.scaleBy fade

            newXYZ =
                trackpoint.xyz
                    |> Point3d.translateBy horizontalVector
                    |> Point3d.translateBy verticalVector

            newProfileXZ =
                trackpoint.profileXZ
                    |> Point3d.translateBy verticalVector
        in
        { trackpoint | xyz = newXYZ, profileXZ = newProfileXZ }


computeNudgedPoints : NudgeSettings -> Track -> List TrackPoint
computeNudgedPoints settings track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        ( fromNode, toNode ) =
            if track.currentNode.index <= markerPosition.index then
                ( track.currentNode, markerPosition )

            else
                ( markerPosition, track.currentNode )

        fadeInStart =
            fromNode.distanceFromStart |> Quantity.minus settings.fadeExtent

        fadeOutEnd =
            toNode.distanceFromStart |> Quantity.plus settings.fadeExtent

        fader pointDistance referenceDistance =
            let
                ( place, base ) =
                    ( inMeters pointDistance, inMeters referenceDistance )

                x =
                    pi * (place - base) / (inMeters settings.fadeExtent)
            in
            (1.0 + cos x) / 2.0

        liesWithin ( lo, hi ) point =
            (point.distanceFromStart |> Quantity.greaterThanOrEqualTo lo)
                && (point.distanceFromStart |> Quantity.lessThanOrEqualTo hi)

        nudge point =
            let
                fade =
                    if liesWithin ( fromNode.distanceFromStart, toNode.distanceFromStart ) point then
                        1.0

                    else if liesWithin ( fadeInStart, fromNode.distanceFromStart ) point then
                        fader point.distanceFromStart fromNode.distanceFromStart

                    else if liesWithin ( toNode.distanceFromStart, fadeOutEnd ) point then
                        fader point.distanceFromStart toNode.distanceFromStart

                    else
                        0.0
            in
            nudgeTrackPoint settings fade point
    in
    List.map nudge track.trackPoints


previewNudgeNodes : NudgeSettings -> Track -> NudgeSettings
previewNudgeNodes settings track =
    -- Change the locations of the track points within the closed interval between
    -- markers, or just the current node if no purple cone.
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        ( fromNode, toNode ) =
            if track.currentNode.index <= markerPosition.index then
                ( track.currentNode, markerPosition )

            else
                ( markerPosition, track.currentNode )

        fadeInStart =
            fromNode.distanceFromStart |> Quantity.minus settings.fadeExtent

        fadeOutEnd =
            toNode.distanceFromStart |> Quantity.plus settings.fadeExtent

        nudgedPoints =
            computeNudgedPoints settings track

        _ = Debug.log "Preview" (previewStartIndex, previewEndIndex)

        previewStartIndex =
            List.Extra.findIndex
                (\p -> p.distanceFromStart |> Quantity.greaterThanOrEqualTo fadeInStart)
                track.trackPoints
                |> Maybe.withDefault from

        previewEndIndex =
            List.Extra.findIndex
                (\p -> p.distanceFromStart |> Quantity.greaterThanOrEqualTo fadeOutEnd)
                track.trackPoints
                |> Maybe.withDefault to

        ( trackBeforePreviewEnd, trackAfterPreviewEnd ) =
            List.Extra.splitAt (previewEndIndex + 2) nudgedPoints

        ( trackBeforePreviewStart, previewZone ) =
            List.Extra.splitAt (previewStartIndex - 1) trackBeforePreviewEnd
    in
    { settings | preview = previewZone }


viewNudgeTools : NudgeSettings -> (NudgeMsg -> msg) -> Element msg
viewNudgeTools settings msgWrapper =
    row [ width fill, spaceEvenly, paddingXY 10 10, spacingXY 10 10 ]
        [ verticalNudgeSlider settings.vertical msgWrapper
        , column [ width fill, spaceEvenly, centerX, paddingXY 10 10, spacingXY 10 10 ]
            [ horizontalNudgeSlider settings.horizontal msgWrapper
            , row [ paddingXY 10 10, spacingXY 10 10 ]
                [ nudgeButton settings msgWrapper
                , zeroButton msgWrapper
                ]
            , row [ paddingXY 10 10, spacingXY 10 10 ]
                [ text "Fade in/out"
                , fadeSlider settings.fadeExtent msgWrapper
                ]
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


fadeSlider : Length -> (NudgeMsg -> msg) -> Element msg
fadeSlider value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetFadeExtent >> wrap
        , label =
            Input.labelBelow [ centerX ] <|
                text <|
                    (showDecimal2 <| Length.inMeters value)
        , min = 0.0
        , max = 50.0
        , step = Just 5.0
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
            ( { settings | horizontal = length }
            , PostUpdateActions.ActionPreview
            )

        SetVerticalNudgeFactor length ->
            ( { settings | vertical = length }
            , PostUpdateActions.ActionPreview
            )

        SetFadeExtent fade ->
            ( { settings | fadeExtent = fade }
            , PostUpdateActions.ActionPreview
            )

        ZeroNudgeFactors ->
            ( defaultNudgeSettings
            , PostUpdateActions.ActionPreview
            )

        NudgeNode _ ->
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
