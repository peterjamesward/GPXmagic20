module Nudge exposing (..)

import Angle
import Axis3d
import Color
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Length, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (UndoEntry)
import Quantity
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints, highlightPointsProfile)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, highlightPoints)
import Utils exposing (showDecimal0, showDecimal2, showShortMeasure)
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
    , fadeExtent : Length.Length
    , nodesToNudge : List TrackPoint
    }


defaultNudgeSettings : NudgeSettings
defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , fadeExtent = Quantity.zero
    , nodesToNudge = []
    }


toolLabel =
    "Nudge"


info =
    """## Nudge

_Note: You may want to try Move & Stretch_

You can adjust a single point or a section of track by
moving it up to 5m up or down, and left or right (relative
to direction of travel).

The view panes will show a preview as an orange line.
The change does not take effect until you click the
"Apply nudge" button"""


makeUndoMessage : Bool -> Track -> String
makeUndoMessage imperial track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( Quantity.min track.currentNode.distanceFromStart markerPosition.distanceFromStart
            , Quantity.max track.currentNode.distanceFromStart markerPosition.distanceFromStart
            )
    in
    if to |> Quantity.greaterThan from then
        "Nudge from "
            ++ showShortMeasure imperial from
            ++ " to "
            ++ showShortMeasure imperial to

    else
        "Nudge node at"
            ++ showShortMeasure imperial from


getPreview : NudgeSettings -> List (Entity LocalCoords)
getPreview model =
    highlightPoints Color.lightOrange model.nodesToNudge


getProfilePreview : NudgeSettings -> List (Entity LocalCoords)
getProfilePreview model =
    highlightPointsProfile Color.lightOrange model.nodesToNudge


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
        ( start, end, section ) =
            Track.getSection track

        ( fromNode, toNode ) =
            ( track.trackPoints |> List.Extra.getAt start |> Maybe.withDefault track.currentNode
            , track.trackPoints |> List.Extra.getAt end |> Maybe.withDefault track.currentNode
            )

        fadeInStart =
            fromNode.distanceFromStart |> Quantity.minus settings.fadeExtent

        fadeOutEnd =
            toNode.distanceFromStart |> Quantity.plus settings.fadeExtent

        fader pointDistance referenceDistance =
            let
                ( place, base ) =
                    ( inMeters pointDistance, inMeters referenceDistance )

                x =
                    abs <| (place - base) / inMeters settings.fadeExtent
            in
            1.0 - x

        actualNudgeRegionIncludingFades =
            track.trackPoints
                |> List.Extra.dropWhile
                    (.distanceFromStart >> Quantity.lessThan fadeInStart)
                |> List.Extra.takeWhile
                    (.distanceFromStart >> Quantity.greaterThanOrEqualTo fadeOutEnd)

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
    List.map nudge actualNudgeRegionIncludingFades


viewNudgeTools : Bool -> NudgeSettings -> (NudgeMsg -> msg) -> Element msg
viewNudgeTools imperial settings msgWrapper =
    row [ width fill, spaceEvenly, paddingXY 10 10, spacingXY 10 10 ]
        [ verticalNudgeSlider imperial settings.vertical msgWrapper
        , column [ width fill, spaceEvenly, centerX, paddingXY 10 10, spacingXY 10 10 ]
            [ horizontalNudgeSlider imperial settings.horizontal msgWrapper
            , row [ paddingXY 10 10, spacingXY 10 10 ]
                [ nudgeButton settings msgWrapper
                , zeroButton msgWrapper
                ]
            , row [ paddingXY 10 10, spacingXY 10 10 ]
                [ text "Fade in/out"
                , fadeSlider imperial settings.fadeExtent msgWrapper
                ]
            ]
        ]


horizontalNudgeSlider : Bool -> Length.Length -> (NudgeMsg -> msg) -> Element msg
horizontalNudgeSlider imperial value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetHorizontalNudgeFactor >> wrap
        , label = Input.labelBelow [ centerX ] <| text <| showShortMeasure imperial value
        , min =
            Length.inMeters <|
                if imperial then
                    Length.feet -16.0

                else
                    Length.meters -5.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 16.0

                else
                    Length.meters 5.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


fadeSlider : Bool -> Length.Length -> (NudgeMsg -> msg) -> Element msg
fadeSlider imperial value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetFadeExtent >> wrap
        , label = Input.labelBelow [ centerX ] <| text <| showShortMeasure imperial value
        , min = 0.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 160.0

                else
                    Length.meters 50.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


verticalNudgeSlider : Bool -> Length.Length -> (NudgeMsg -> msg) -> Element msg
verticalNudgeSlider imperial value wrap =
    el [ width <| px 80, centerX ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = Length.meters >> SetVerticalNudgeFactor >> wrap
            , label = Input.labelLeft [ centerY ] <| text <| showShortMeasure imperial value
            , min =
                Length.inMeters <|
                    if imperial then
                        Length.feet -16.0

                    else
                        Length.meters -5.0
            , max =
                Length.inMeters <|
                    if imperial then
                        Length.feet 16.0

                    else
                        Length.meters 5.0
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
    -> ( NudgeSettings, PostUpdateActions.PostUpdateAction Track msg )
update msg settings track =
    case msg of
        SetHorizontalNudgeFactor length ->
            ( { settings
                | horizontal = length
                , nodesToNudge = computeNudgedPoints settings track
              }
            , PostUpdateActions.ActionPreview
            )

        SetVerticalNudgeFactor length ->
            ( { settings
                | vertical = length
                , nodesToNudge = computeNudgedPoints settings track
              }
            , PostUpdateActions.ActionPreview
            )

        SetFadeExtent fade ->
            ( { settings
                | fadeExtent = fade
                , nodesToNudge = computeNudgedPoints settings track
              }
            , PostUpdateActions.ActionPreview
            )

        ZeroNudgeFactors ->
            ( defaultNudgeSettings
            , PostUpdateActions.ActionPreview
            )

        NudgeNode _ ->
            let
                ( startIndex, endIndex, section ) =
                    Track.getSection track

                actionEntry : UndoEntry Track
                actionEntry =
                    { label = makeUndoMessage track
                    , firstChangedPoint = startIndex
                    , lastChangedPoint = endIndex
                    , oldTrackpoints = section
                    , editFunction = computeNudgedPoints settings
                    }
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                actionEntry
            )


settingNotZero : NudgeSettings -> Bool
settingNotZero settings =
    Length.inMeters settings.horizontal
        /= 0.0
        || Length.inMeters settings.vertical
        /= 0.0
