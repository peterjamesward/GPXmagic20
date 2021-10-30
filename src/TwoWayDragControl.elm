module TwoWayDragControl exposing (..)

import Direction2d
import Element exposing (..)
import Element.Input as Input
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import PostUpdateActions
import Quantity
import Svg
import Svg.Attributes as SA
import TabCommonElements exposing (markerTextHelper)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showAngle, showDecimal2, showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (checkboxIcon, edges)


type Mode
    = Translate
    | Stretch


type alias Model =
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    , preview : List TrackPoint
    , mode : Mode
    , stretchPointer : Maybe Int
    }


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


defaultModel =
    { vector = Vector2d.zero
    , dragging = Nothing
    , preview = []
    , mode = Translate
    , stretchPointer = Nothing
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool


radius =
    100


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


settingNotZero : Model -> Bool
settingNotZero model =
    Vector2d.direction model.vector /= Nothing


twoWayDragControl : Model -> (Msg -> msg) -> Element msg
twoWayDragControl model wrapper =
    let
        clickableContainer =
            el
                [ htmlAttribute <| Pointer.onDown (.pointer >> .offsetPos >> point >> DraggerGrab >> wrapper)
                , htmlAttribute <| Pointer.onMove (.pointer >> .offsetPos >> point >> DraggerMove >> wrapper)
                , htmlAttribute <| Pointer.onUp (.pointer >> .offsetPos >> point >> DraggerRelease >> wrapper)
                , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                , Element.width Element.fill
                , Element.pointer
                ]
                << html
                << Svg.svg
                    [ SA.viewBox "-150 -150 300 300"
                    , SA.width "140px"
                    , SA.height "140px"
                    ]

        ( x, y ) =
            Vector2d.components model.vector

        ( xPoint, yPoint ) =
            ( String.fromFloat <| Length.inMeters x
            , String.fromFloat <| Length.inMeters y
            )
    in
    clickableContainer <|
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromInt radius
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 xPoint
            , SA.y2 yPoint
            , SA.stroke "orange"
            , SA.strokeWidth "10"
            , SA.strokeLinecap "round"
            ]
            []
        ]


update :
    Msg
    -> Model
    -> (Msg -> msg)
    -> Track
    -> ( Model, PostUpdateActions.PostUpdateAction (Cmd msg) )
update message model wrapper track =
    case message of
        DraggerGrab offset ->
            ( { model | dragging = Just offset }
            , PostUpdateActions.ActionNoOp
            )

        DraggerMove offset ->
            case model.dragging of
                Nothing ->
                    ( model
                    , PostUpdateActions.ActionPreview
                    )

                Just dragStart ->
                    let
                        newVector =
                            Vector2d.from dragStart offset
                    in
                    ( { model
                        | vector =
                            if Vector2d.length newVector |> Quantity.greaterThan (meters 100.0) then
                                Vector2d.scaleTo (Length.meters 100.0) newVector

                            else
                                newVector
                      }
                    , PostUpdateActions.ActionPreview
                    )

        DraggerRelease _ ->
            ( { model | dragging = Nothing }
            , PostUpdateActions.ActionPreview
            )

        DraggerModeToggle bool ->
            ( { model
                | mode =
                    case model.mode of
                        Translate ->
                            Stretch

                        Stretch ->
                            Translate
              }
            , PostUpdateActions.ActionPreview
            )


view : Bool -> Model -> (Msg -> msg) -> Track -> Element msg
view imperial model wrapper track =
    -- TODO: Try with linear vector, switch to log or something else if needed.
    let
        directionString =
            case Vector2d.direction model.vector of
                Just direction ->
                    showAngle <| Direction2d.toAngle direction

                Nothing ->
                    "none"

        magnitudeString =
            showShortMeasure imperial <| Vector2d.length model.vector
    in
    row [ paddingEach { edges | right = 10 } ]
        [ twoWayDragControl model wrapper
        , column [ Element.alignLeft, Element.width Element.fill ]
            [ markerTextHelper track
            , Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = checkboxIcon
                , checked = model.mode == Stretch
                , label = Input.labelRight [ centerY ] (text "Stretch")
                }
            , Element.text magnitudeString
            , Element.text directionString
            ]
        ]


info : String
info =
    """## Move & Stretch

It's the new Nudge. Bracket some track with the markers and use the cool dragging control to
move the track section. You will have to fix the transitions later.

In Stretch mode, you see a new Cyan pointer. The control will move the Cyan pointer and the
sections of track either side will expand or contract to follow it. This could be used for
separating hairpins, or just to avoid a close pass, or because you can.
"""


preview : Model -> Track -> Model
preview model track =
    -- Change the locations of the track points within the closed interval between
    -- markers, or just the current node if no purple cone.
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        previewTrackPoints =
            computeNewPoints model track

        ( trackBeforePreviewEnd, trackAfterPreviewEnd ) =
            List.Extra.splitAt (to + 2) previewTrackPoints

        ( trackBeforePreviewStart, previewZone ) =
            List.Extra.splitAt (from - 1) trackBeforePreviewEnd
    in
    { model | preview = previewZone }


computeNewPoints : Model -> Track -> List TrackPoint
computeNewPoints model track =
    -- This used by preview and action.
    let
        ( x, y ) =
            Vector2d.components model.vector

        translation =
            Point3d.translateBy (Vector3d.xyz x y (meters 0))

        newPoint trackpoint =
            let
                newXYZ =
                    translation trackpoint.xyz
            in
            { trackpoint | xyz = newXYZ }

        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        ( beforeEnd, afterEnd ) =
            List.Extra.splitAt (to - 1) track.trackPoints

        ( beforeStart, affectedRegion ) =
            List.Extra.splitAt from beforeEnd

        adjustedPoints =
            List.map newPoint affectedRegion
    in
    beforeStart ++ adjustedPoints ++ afterEnd
