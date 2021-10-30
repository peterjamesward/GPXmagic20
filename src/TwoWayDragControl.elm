module TwoWayDragControl exposing (..)

import Angle exposing (Angle)
import Direction2d
import Element exposing (..)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import LocalCoords exposing (LocalCoords)
import Point2d
import PostUpdateActions
import Quantity
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import TabCommonElements exposing (markerTextHelper)
import Track exposing (Track)
import TrackPoint
import Utils exposing (showAngle, showDecimal2, showShortMeasure)
import Vector2d


type alias Model =
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    }


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


defaultModel =
    { vector = Vector2d.zero
    , dragging = Nothing
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point


radius =
    100


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


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
                << svg
                    [ viewBox "-150 -150 300 300"
                    , S.width "140px"
                    , S.height "140px"
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
            [ cx "0"
            , cy "0"
            , r <| String.fromInt radius
            , stroke "black"
            , strokeWidth "1"
            , S.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ x1 "0"
            , y1 "0"
            , x2 xPoint
            , y2 yPoint
            , stroke "orange"
            , strokeWidth "10"
            , strokeLinecap "round"
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
                    , PostUpdateActions.ActionNoOp
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
                    , PostUpdateActions.ActionNoOp
                    )

        DraggerRelease _ ->
            ( { model | dragging = Nothing }
            , PostUpdateActions.ActionNoOp
            )


view : Bool -> Model -> (Msg -> msg) -> Track -> Element msg
view imperial model wrapper track  =
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
    row [  ]
        [ twoWayDragControl model wrapper
        , column [ Element.alignLeft, Element.width Element.fill ]
            [ markerTextHelper track
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
