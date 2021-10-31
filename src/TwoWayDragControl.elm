module TwoWayDragControl exposing (..)

import Axis3d
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
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Vector2d
import Vector3d
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, edges, prettyButtonStyles)


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
    | DraggerReset
    | DraggerMarker Int
    | DraggerApply


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
                    ( track
                        |> preview
                            { model
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

        DraggerReset ->
            ( { model
                | dragging = Nothing
                , vector = Vector2d.zero
                , preview = []
              }
            , PostUpdateActions.ActionPreview
            )

        DraggerMarker int ->
            ( { model | stretchPointer = Just int }
            , PostUpdateActions.ActionPreview
            )

        DraggerApply ->
            ( { model | preview = [] }
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (apply track model)
                (makeUndoMessage model)
            )


minmax a b =
    ( toFloat <| min a b
    , toFloat <| max a b
    )


view : Bool -> Model -> (Msg -> msg) -> Track -> Element msg
view imperial model wrapper track =
    let
        canApply =
            case track.markedNode of
                Just purple ->
                    let
                        ( from, to ) =
                            minmax track.currentNode.index purple.index
                    in
                    case model.mode of
                        Translate ->
                            from < to

                        Stretch ->
                            let
                                drag =
                                    model.stretchPointer |> Maybe.withDefault 0 |> toFloat
                            in
                            from < drag && drag < to

                Nothing ->
                    False
    in
    -- Try with linear vector, switch to log or something else if needed.
    row [ paddingEach { edges | right = 10 } ]
        [ twoWayDragControl model wrapper
        , column
            [ Element.alignLeft
            , Element.width Element.fill
            , spacing 5
            ]
            [ markerTextHelper track
            , Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = checkboxIcon
                , checked = model.mode == Stretch
                , label = Input.labelRight [ centerY ] (text "Stretch")
                }
            , case ( model.mode, track.markedNode ) of
                ( Stretch, Just purple ) ->
                    let
                        ( from, to ) =
                            minmax track.currentNode.index purple.index
                    in
                    Input.slider commonShortHorizontalSliderStyles
                        { onChange = wrapper << DraggerMarker << round
                        , label =
                            Input.labelBelow []
                                (text "Choose the point to drag")
                        , min = from
                        , max = to
                        , step = Just 1.0
                        , value = model.stretchPointer |> Maybe.withDefault 0 |> toFloat
                        , thumb = Input.defaultThumb
                        }

                _ ->
                    none
            , row [ spacing 5 ]
                [ Input.button prettyButtonStyles
                    { label = text "Zero", onPress = Just <| wrapper DraggerReset }
                , if canApply then
                    Input.button prettyButtonStyles
                        { label = text "Apply"
                        , onPress = Just <| wrapper DraggerApply
                        }

                  else
                    Input.button prettyButtonStyles
                        { label = text "Not valid"
                        , onPress = Nothing
                        }
                ]
            ]
        ]


info : String
info =
    """## Move & Stretch

It's the new Nudge. Bracket some track with the markers and use the cool dragging control to
move the track section. You will have to fix the transitions later.

In Stretch mode, you see a new White pointer. The control will move the White pointer and the
sections of track either side will expand or contract to follow it. This could be used for
separating hairpins, or just to avoid a close pass, or because you can.
"""


apply : Track -> Model -> Track
apply track model =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        newTrackPoints =
            case model.mode of
                Translate ->
                    movePoints model track

                Stretch ->
                    stretchPoints model track

        newCurrent =
            List.Extra.getAt track.currentNode.index newTrackPoints
                |> Maybe.withDefault track.currentNode

        newMarker =
            case track.markedNode of
                Just isMarked ->
                    List.Extra.getAt isMarked.index newTrackPoints

                Nothing ->
                    Nothing
    in
    { track
        | trackPoints = newTrackPoints
        , currentNode = newCurrent
        , markedNode = newMarker
    }


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
            case model.mode of
                Translate ->
                    movePoints model track

                Stretch ->
                    stretchPoints model track

        ( trackBeforePreviewEnd, trackAfterPreviewEnd ) =
            List.Extra.splitAt (to + 2) previewTrackPoints

        ( trackBeforePreviewStart, previewZone ) =
            List.Extra.splitAt (from - 1) trackBeforePreviewEnd
    in
    { model | preview = previewZone }


movePoints : Model -> Track -> List TrackPoint
movePoints model track =
    -- This used by preview and action.
    let
        ( x, y ) =
            Vector2d.components model.vector

        translation =
            -- Negate y because SVG coordinates go downards.
            Point3d.translateBy (Vector3d.xyz x (Quantity.negate y) (meters 0))

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
            List.Extra.splitAt (to + 1) track.trackPoints

        ( beforeStart, affectedRegion ) =
            List.Extra.splitAt from beforeEnd

        adjustedPoints =
            List.map newPoint affectedRegion
    in
    beforeStart ++ adjustedPoints ++ afterEnd


stretchPoints : Model -> Track -> List TrackPoint
stretchPoints model track =
    -- This used by preview and action.
    -- Here we move points either side of the stretch marker.
    let
        ( x, y ) =
            Vector2d.components model.vector

        translation =
            -- Negate y because SVG coordinates go downards.
            Vector3d.xyz x (Quantity.negate y) (meters 0)

        -- Point2d.signedDistanceAlong and .signedDistanceFrom give axis relative coordinates.
        -- Then we need only translate along the axis by the proportionate distanceAlong, so we're home.
        marker =
            track.markedNode |> Maybe.withDefault track.currentNode

        referenceIdx =
            case model.stretchPointer of
                Just idx ->
                    idx

                Nothing ->
                    -- Won't happen but have to do this
                    track.currentNode.index

        referencePoint =
            List.Extra.getAt referenceIdx track.trackPoints
                |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index marker.index
            , max track.currentNode.index marker.index
            )

        ( startAnchor, endAnchor ) =
            ( track.trackPoints |> List.Extra.getAt from |> Maybe.withDefault track.currentNode
            , track.trackPoints |> List.Extra.getAt to |> Maybe.withDefault track.currentNode
            )

        ( beforeEnd, afterEnd ) =
            track.trackPoints |> List.Extra.splitAt (to + 1)

        ( beforeReference, secondPart ) =
            beforeEnd |> List.Extra.splitAt referenceIdx

        ( beforeStart, firstPart ) =
            beforeReference |> List.Extra.splitAt from

        ( firstPartAxis, secondPartAxis ) =
            ( Axis3d.throughPoints startAnchor.xyz referencePoint.xyz
            , Axis3d.throughPoints endAnchor.xyz referencePoint.xyz
            )

        ( firstPartDistance, secondPartDistance ) =
            ( Point3d.distanceFrom startAnchor.xyz referencePoint.xyz
            , Point3d.distanceFrom endAnchor.xyz referencePoint.xyz
            )

        distanceAlong maybeAxis p =
            case maybeAxis of
                Just axis ->
                    p |> Point3d.signedDistanceAlong axis

                Nothing ->
                    Quantity.zero

        ( adjustedFirstPoints, adjustedSecondPoints ) =
            ( List.map adjustRelativeToStart firstPart
            , List.map adjustRelativeToEnd secondPart
            )

        adjustRelativeToStart pt =
            let
                proportion =
                    Quantity.ratio
                        (pt.xyz |> distanceAlong firstPartAxis)
                        firstPartDistance
            in
            { pt
                | xyz =
                    pt.xyz |> Point3d.translateBy (translation |> Vector3d.scaleBy proportion)
            }

        adjustRelativeToEnd pt =
            let
                proportion =
                    Quantity.ratio
                        (pt.xyz |> distanceAlong secondPartAxis)
                        secondPartDistance
            in
            { pt
                | xyz =
                    pt.xyz |> Point3d.translateBy (translation |> Vector3d.scaleBy proportion)
            }
    in
    -- Avoid potential division by zero.
    if from < referenceIdx && referenceIdx < to then
        beforeStart ++ adjustedFirstPoints ++ adjustedSecondPoints ++ afterEnd

    else
        track.trackPoints


getStretchPointer : Model -> Track -> Maybe TrackPoint
getStretchPointer model track =
    case ( model.mode, model.stretchPointer ) of
        ( Stretch, Just n ) ->
            List.Extra.getAt n track.trackPoints

        _ ->
            Nothing


makeUndoMessage : Model -> String
makeUndoMessage model =
    case model.mode of
        Translate ->
            "Slide a section of track"

        Stretch ->
            "Stretch a section of track"
