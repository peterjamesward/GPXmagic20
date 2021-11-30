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
import PostUpdateActions exposing (UndoEntry, defaultUndoEntry)
import Quantity
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints, highlightPointsProfile, previewLine)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showShortMeasure)
import Vector3d
import ViewPureStyles exposing (..)


type NudgeMsg
    = SetHorizontalNudgeFactor Length.Length
    | SetVerticalNudgeFactor Length.Length
    | ZeroNudgeFactors
    | NudgeNode NudgeSettings
    | SetFadeExtent Length.Length


type alias NudgeSettings =
    { horizontal : Length.Length
    , vertical : Length.Length
    , fadeExtent : Length.Length

    --, action : UndoEntry
    }


type alias UndoRedoInfo =
    { horizontal : Length.Length
    , vertical : Length.Length
    , fadeExtent : Length.Length
    , orange : Int
    , purple : Maybe Int
    }


defaultNudgeSettings : NudgeSettings
defaultNudgeSettings =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , fadeExtent = Quantity.zero

    --, action = defaultUndoEntry
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


getPreview : NudgeSettings -> Track -> List (Entity LocalCoords)
getPreview settings track =
    let
        undoEntry =
            buildActions False settings track

        ( _, nudged, _ ) =
            undoEntry.editFunction track
    in
    previewLine Color.yellow nudged


getProfilePreview : NudgeSettings -> Track -> List (Entity LocalCoords)
getProfilePreview settings track =
    let
        undoEntry =
            buildActions False settings track

        ( _, nudged, _ ) =
            undoEntry.editFunction track
    in
    highlightPointsProfile Color.lightOrange nudged


nudgeTrackPoint : UndoRedoInfo -> Float -> TrackPoint -> TrackPoint
nudgeTrackPoint undoRedoInfo fade trackpoint =
    if fade == 0 then
        trackpoint

    else
        let
            horizontalDirection =
                trackpoint.effectiveDirection
                    |> Maybe.withDefault Direction3d.x
                    |> Direction3d.rotateAround Axis3d.z (Angle.degrees -90)

            horizontalVector =
                Vector3d.withLength undoRedoInfo.horizontal horizontalDirection
                    |> Vector3d.scaleBy fade

            verticalVector =
                Vector3d.xyz Quantity.zero Quantity.zero undoRedoInfo.vertical
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


splitTheTrackAllowingForFade :
    UndoRedoInfo
    -> Track
    -> ( List TrackPoint, List TrackPoint, List TrackPoint )
splitTheTrackAllowingForFade undoRedoInfo track =
    let
        ( _, _, section ) =
            case track.markedNode of
                Just _ ->
                    Track.getSection track

                Nothing ->
                    ( track.currentNode.index
                    , track.currentNode.index
                    , [ track.currentNode ]
                    )

        ( fromNode, toNode ) =
            ( section |> List.head |> Maybe.withDefault track.currentNode
            , section |> List.Extra.last |> Maybe.withDefault track.currentNode
            )

        fadeInStart =
            fromNode.distanceFromStart |> Quantity.minus undoRedoInfo.fadeExtent

        fadeOutEnd =
            toNode.distanceFromStart |> Quantity.plus undoRedoInfo.fadeExtent

        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitWhen
                    (.distanceFromStart >> Quantity.greaterThanOrEqualTo fadeInStart)
                |> Maybe.withDefault ( [], track.trackPoints )

        ( actualNudgeRegionIncludingFades, suffix ) =
            theRest
                |> List.Extra.splitWhen
                    (.distanceFromStart >> Quantity.greaterThan fadeOutEnd)
                |> Maybe.withDefault ( theRest, [] )
    in
    ( prefix, actualNudgeRegionIncludingFades, suffix )


editFunction :
    UndoRedoInfo
    -> Track
    -> ( List TrackPoint, List TrackPoint, List TrackPoint )
editFunction undoRedoInfo track =
    -- This is when the Nudge actually takes effect. It can be called to create preview
    -- or for the real McCoy.
    let
        ( prefix, actualNudgeRegionIncludingFades, suffix ) =
            -- Yes, we split it again for the actual edit or Redo.
            splitTheTrackAllowingForFade undoRedoInfo track

        fader pointDistance referenceDistance =
            let
                ( place, base ) =
                    ( inMeters pointDistance, inMeters referenceDistance )

                x =
                    abs <| (place - base) / inMeters undoRedoInfo.fadeExtent
            in
            1.0 - x

        ( fromNode, toNode ) =
            ( actualNudgeRegionIncludingFades |> List.head |> Maybe.withDefault track.currentNode
            , actualNudgeRegionIncludingFades |> List.Extra.last |> Maybe.withDefault track.currentNode
            )

        fadeInStart =
            fromNode.distanceFromStart |> Quantity.minus undoRedoInfo.fadeExtent

        fadeOutEnd =
            toNode.distanceFromStart |> Quantity.plus undoRedoInfo.fadeExtent

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
            nudgeTrackPoint undoRedoInfo fade point
    in
    ( prefix
    , List.map nudge actualNudgeRegionIncludingFades
    , suffix
    )


undoFunction :
    ( Int, Int )
    -> List TrackPoint
    -> Track
    -> ( List TrackPoint, List TrackPoint, List TrackPoint )
undoFunction ( start, end ) savedPoints track =
    -- Construct the closure for undo-ing.
    -- No need to save the whole track, only the bit we've Nudged.
    let
        ( prefix, theRest ) =
            track.trackPoints |> List.Extra.splitAt start

        ( middle, suffix ) =
            theRest |> List.Extra.splitAt (end - start)
    in
    ( prefix, savedPoints, suffix )


buildActions : Bool -> NudgeSettings -> Track -> UndoEntry
buildActions imperial settings track =
    -- Here we simply create closures that can be called to apply, undo, redo the edit.
    let
        storedSettings : UndoRedoInfo
        storedSettings =
            -- Make sure our closure values are ours.
            { horizontal = settings.horizontal
            , vertical = settings.vertical
            , fadeExtent = settings.fadeExtent
            , orange = track.currentNode.index
            , purple = Maybe.map .index track.markedNode
            }

        ( prefix, actualNudgeRegionIncludingFades, suffix ) =
            -- Have to split it here to get the trackpoints to save for Undo.
            splitTheTrackAllowingForFade storedSettings track

        ( start, end ) =
            ( actualNudgeRegionIncludingFades
                |> List.head
                |> Maybe.withDefault track.currentNode
                |> .index
            , suffix
                |> List.head
                |> Maybe.withDefault track.currentNode
                |> .index
            )
    in
    -- This is the +/-ve delta for possible redo. We do not include track in the closure!
    { label = makeUndoMessage imperial track
    , editFunction = editFunction storedSettings
    , undoFunction = undoFunction ( start, end ) actualNudgeRegionIncludingFades
    }


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
    -> Bool
    -> NudgeSettings
    -> Track
    -> ( NudgeSettings, PostUpdateActions.PostUpdateAction Track msg )
update msg imperial settings track =
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
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildActions imperial settings track)
            )
