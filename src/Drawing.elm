module Drawing exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Color exposing (Color)
import Html exposing (Html, button, div, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode



--main : Program Float Model Msg
--main =
--    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> (Msg -> msg) -> Sub msg
subscriptions model msgWrapper =
    onAnimationFrameDelta (AnimationFrame >> msgWrapper)


h : number
h =
    500


w : number
w =
    500


padding : number
padding =
    20


type alias DrawingPointer =
    { previousMidpoint : Point, lastPoint : Point }


type alias Model =
    { frames : Int
    , pending : Array Renderable
    , toDraw : List Renderable
    , drawingPointer : Maybe DrawingPointer
    , color : Color
    , size : Int
    }


type Msg
    = AnimationFrame Float
    | StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )
    | SelectColor Color
    | SelectSize Int


init : Model
init =
    { frames = 0
    , pending = Array.empty
    , toDraw = []
    , drawingPointer = Nothing
    , color = Color.lightBlue
    , size = 20
    }


update : Msg -> Model -> Model
update msg ({ frames, drawingPointer, pending, toDraw } as model) =
    case msg of
        AnimationFrame delta ->
            model
                |> incFrames
                |> flushPendingToDraw

        StartAt point ->
            initialPoint point model

        MoveAt point ->
            case drawingPointer of
                Just pointer ->
                    drawPoint point pointer model

                Nothing ->
                    model

        EndAt point ->
            case drawingPointer of
                Just pointer ->
                    finalPoint point pointer model

                Nothing ->
                    model

        SelectColor color ->
            selectColor color model

        SelectSize size ->
            selectSize size model


incFrames ({ frames } as model) =
    { model | frames = frames + 1 }


flushPendingToDraw ({ pending } as model) =
    { model
        | pending = Array.empty
        , toDraw = Array.toList pending
    }


selectColor color model =
    { model | color = color }


selectSize size model =
    { model | size = size }


initialPoint (( x, y ) as point) model =
    { model
        | drawingPointer = Just { previousMidpoint = ( x, y ), lastPoint = ( x, y ) }
    }


drawPoint newPoint { previousMidpoint, lastPoint } ({ pending } as model) =
    let
        newMidPoint =
            controlPoint lastPoint newPoint
    in
    { model
        | drawingPointer = Just { previousMidpoint = newMidPoint, lastPoint = newPoint }
        , pending =
            Array.push
                (drawLine model
                    [ path previousMidpoint [ quadraticCurveTo lastPoint newMidPoint ] ]
                )
                pending
    }


finalPoint point { previousMidpoint, lastPoint } ({ pending } as model) =
    { model
        | drawingPointer = Nothing
        , pending =
            Array.push
                (drawLine model
                    [ path previousMidpoint [ quadraticCurveTo lastPoint point ] ]
                )
                pending
    }


controlPoint ( x1, y1 ) ( x2, y2 ) =
    ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )


drawLine : Model -> List Shape -> Renderable
drawLine { color, size } line =
    line
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth (toFloat size)
            , shadow { blur = 10, offset = ( 0, 0 ), color = getShadowColor color }
            , stroke color
            ]


getShadowColor color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Color.rgba red green blue 0.2


view : Model -> (Msg -> msg) -> Html msg
view { color, size, toDraw } msgWrapper =
    div []
        [ p [ style "text-align" "center", style "font-size" "80%" ]
            [ Html.text "Draw something! (mouse or touch)"
            ]
        , Canvas.toHtml ( w, h )
            [ style "touch-action" "none"
            , Mouse.onDown (.offsetPos >> StartAt >> msgWrapper)
            , Mouse.onMove (.offsetPos >> MoveAt >> msgWrapper)
            , Mouse.onUp (.offsetPos >> EndAt >> msgWrapper)

            -- These 2 get annoying sometimes when painting
            -- , Mouse.onLeave (.offsetPos >> EndAt)
            -- , Mouse.onContextMenu (.offsetPos >> EndAt)
            , onTouch "touchstart" (touchCoordinates >> StartAt >> msgWrapper)
            , onTouch "touchmove" (touchCoordinates >> MoveAt >> msgWrapper)
            , onTouch "touchend" (touchCoordinates >> EndAt >> msgWrapper)
            ]
            toDraw
        , div
            [ style "max-width" (String.fromInt (w - 20) ++ "px")
            , style "padding" "10px"
            ]
            [ sizeControls color size msgWrapper
            , colorButtons color msgWrapper
            ]
        ]


sizeControls selectedColor selectedSize msgWrapper =
    let
        brushes =
            6

        inc =
            10

        buttonSize =
            brushes * inc

        controls =
            List.range 0 brushes
                |> List.map
                    (\i ->
                        let
                            size =
                                max 2 (i * inc)
                        in
                        button
                            [ style "-webkit-appearance" "none"
                            , style "-moz-appearance" "none"
                            , style "display" "block"
                            , style "background-color" "transparent"
                            , style "border" "none"
                            , style "margin" "5px"
                            , style "padding" "0"
                            , style "min-width" (String.fromInt 30 ++ "px")
                            , style "min-height" (String.fromInt buttonSize ++ "px")
                            , style "outline" "none"
                            , onClick <| msgWrapper <| SelectSize size
                            ]
                            [ div
                                [ style "border-radius" "50%"
                                , style "background-color" (Color.toCssString selectedColor)
                                , style "border" ("3px solid " ++ (Color.white |> getShadowColor |> Color.toCssString))
                                , style "width" (String.fromInt size ++ "px")
                                , style "height" (String.fromInt size ++ "px")
                                , style "margin" "0 auto"
                                , style "box-shadow"
                                    (if selectedSize == size then
                                        "rgba(0, 0, 0, 0.4) 0px 4px 6px"

                                     else
                                        "none"
                                    )
                                , style "transition" "transform 0.2s linear"
                                , style "transform"
                                    (if selectedSize == size then
                                        "translateY(-6px)"

                                     else
                                        "none"
                                    )
                                ]
                                []
                            ]
                    )
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-around"
        , style "align-items" "center"
        ]
        controls


colorButtons selectedColor msgWrapper =
    let
        layout colors =
            colors
                |> List.map (List.map (colorButton msgWrapper selectedColor) >> col)
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-around"
        ]
    <|
        layout
            [ [ Color.lightRed
              , Color.red
              , Color.darkRed
              ]
            , [ Color.lightOrange
              , Color.orange
              , Color.darkOrange
              ]
            , [ Color.lightYellow
              , Color.yellow
              , Color.darkYellow
              ]
            , [ Color.lightGreen
              , Color.green
              , Color.darkGreen
              ]
            , [ Color.lightBlue
              , Color.blue
              , Color.darkBlue
              ]
            , [ Color.lightPurple
              , Color.purple
              , Color.darkPurple
              ]
            , [ Color.lightBrown
              , Color.brown
              , Color.darkBrown
              ]
            , [ Color.white
              , Color.lightGrey
              , Color.grey
              ]
            , [ Color.darkGrey
              , Color.lightCharcoal
              , Color.charcoal
              ]
            , [ Color.darkCharcoal
              , Color.black
              ]
            ]


col btns =
    div [] btns


colorButton msgWrapper selectedColor color =
    button
        [ style "border-radius" "50%"
        , style "background-color" (Color.toCssString color)
        , style "display" "block"
        , style "width" "40px"
        , style "height" "40px"
        , style "margin" "5px"
        , style "border" "2px solid white"
        , style "box-shadow"
            (if selectedColor == color then
                "rgba(0, 0, 0, 0.4) 0px 4px 6px"

             else
                "none"
            )
        , style "transition" "transform 0.2s linear"
        , style "outline" "none"
        , style "transform"
            (if selectedColor == color then
                "translateY(-6px)"

             else
                "none"
            )
        , onClick <| msgWrapper <| (SelectColor color)
        ]
        []


touchCoordinates : { event : Touch.Event, targetOffset : ( Float, Float ) } -> ( Float, Float )
touchCoordinates { event, targetOffset } =
    List.head event.changedTouches
        |> Maybe.map
            (\touch ->
                let
                    ( x, y ) =
                        touch.pagePos

                    ( x2, y2 ) =
                        targetOffset
                in
                ( x - x2, y - y2 )
            )
        |> Maybe.withDefault ( 0, 0 )


onTouch event tag =
    eventDecoder
        |> Decode.map
            (\ev ->
                { message = tag ev
                , preventDefault = True
                , stopPropagation = True
                }
            )
        |> Html.Events.custom event


eventDecoder =
    Decode.map2
        (\event offset ->
            { event = event
            , targetOffset = offset
            }
        )
        Touch.eventDecoder
        offsetDecoder


offsetDecoder =
    Decode.field "target"
        (Decode.map2 (\top left -> ( left, top ))
            (Decode.field "offsetTop" Decode.float)
            (Decode.field "offsetLeft" Decode.float)
        )
