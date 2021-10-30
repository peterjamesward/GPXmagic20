module TwoWayDragControl exposing (..)

import Angle exposing (Angle)
import Element exposing (..)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length
import LocalCoords exposing (LocalCoords)
import Point2d
import PostUpdateActions
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Track exposing (Track)
import TrackPoint
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
    135


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
                    [ viewBox "-200 -200 400 400"
                    , S.width "300px"
                    , S.height "300px"
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
            , r "140"
            , stroke "grey"
            , strokeWidth "1"
            , S.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ x1 "0"
            , y1 "0"
            , x2 xPoint
            , y2 yPoint
            , stroke "antiquewhite"
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

                Just prevDragPoint ->
                    ( { model
                        | dragging = Just offset
                        , vector = Vector2d.from prevDragPoint offset
                      }
                    , PostUpdateActions.ActionNoOp
                    )

        DraggerRelease _ ->
            ( { model | dragging = Nothing }
            , PostUpdateActions.ActionNoOp
            )


view : Bool -> Model -> (Msg -> msg) -> Element msg
view imperial model wrapper =
    Element.text "TODO"


info : String
info =
    """## Move & Stretch

It's the new Nudge. Bracket some track with the markers and use the cool dragging control to
move the track section. You will have to fix the transitions later.

In Stretch mode, you see a new Cyan pointer. The control will move the Cyan pointer and the
sections of track either side will expand or contract to follow it. This could be used for
separating hairpins, or just to avoid a close pass, or because you can.
"""
