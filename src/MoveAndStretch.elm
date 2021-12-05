module MoveAndStretch exposing (..)

import Axis3d
import Color
import Direction3d
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Input as Input
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Json.Encode as E
import Length exposing (meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import PostUpdateActions exposing (UndoEntry)
import Quantity
import Scene3d exposing (Entity)
import SceneBuilder exposing (previewLine)
import SceneBuilderProfile exposing (previewProfileLine)
import Svg
import Svg.Attributes as SA
import TabCommonElements exposing (markerTextHelper)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, commonShortVerticalSliderStyles, edges, prettyButtonStyles)


toolLabel =
    "Move & Stretch"


type Mode
    = Translate
    | Stretch


type alias Model =
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , lastVector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    , preview : List TrackPoint
    , mode : Mode
    , stretchPointer : Maybe Int
    , heightSliderSetting : Float
    }


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


defaultModel =
    { vector = Vector2d.zero
    , lastVector = Vector2d.zero
    , dragging = Nothing
    , preview = []
    , mode = Translate
    , stretchPointer = Nothing
    , heightSliderSetting = 0.0
    }


type alias UndoRedoInfo =
    { regionStart : Int
    , regionEnd : Int
    , originalPoints : List (Point3d.Point3d Length.Meters LocalCoords)
    , revisedPoints : List (Point3d.Point3d Length.Meters LocalCoords)
    , originalProfileXZ : List (Point3d.Point3d Length.Meters LocalCoords)
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool
    | DraggerReset
    | DraggerMarker Int
    | DraggerApply
    | StretchHeight Float


radius =
    100


heightOffset sliderValue =
    -- Using cube here gives large range, preserves sign, more control in the centre.
    let
        clamped =
            -- Just to be sure.
            clamp -1.0 1.0 sliderValue
    in
    Length.meters <| 100.0 * clamped * clamped * clamped


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


settingNotZero : Model -> Bool
settingNotZero model =
    Vector2d.direction model.vector
        /= Nothing
        || model.heightSliderSetting
        /= 0.0


twoWayDragControl : Model -> (Msg -> msg) -> Element msg
twoWayDragControl model wrapper =
    let
        clickableContainer =
            el
                [ htmlAttribute <| Pointer.onDown (.pointer >> .offsetPos >> point >> DraggerGrab >> wrapper)
                , htmlAttribute <| Pointer.onMove (.pointer >> .offsetPos >> point >> DraggerMove >> wrapper)
                , htmlAttribute <| Pointer.onUp (.pointer >> .offsetPos >> point >> DraggerRelease >> wrapper)
                , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                , Element.pointer
                , Element.alignLeft
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
    -> ( Model, PostUpdateActions.PostUpdateAction trck (Cmd msg) )
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
                            model.lastVector |> Vector2d.plus (Vector2d.from dragStart offset)
                    in
                    ( { model | vector = newVector }
                    , PostUpdateActions.ActionPreview
                    )

        DraggerRelease _ ->
            ( { model
                | dragging = Nothing
                , lastVector = model.vector
              }
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
                , heightSliderSetting = 0.0
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
                (buildActions model track)
            )

        StretchHeight x ->
            ( { model | heightSliderSetting = x }
            , PostUpdateActions.ActionPreview
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

        heightSlider =
            Input.slider commonShortVerticalSliderStyles
                { onChange = wrapper << StretchHeight
                , label =
                    Input.labelBelow [ alignRight ]
                        (text <| showShortMeasure imperial (heightOffset model.heightSliderSetting))
                , min = -1.0
                , max = 1.0
                , step = Nothing
                , value = model.heightSliderSetting
                , thumb =
                    Input.defaultThumb
                }

        showSliderInStretchMode =
            case ( model.mode, track.markedNode ) of
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
                        , min = from + 1
                        , max = to - 1
                        , step = Just 1.0
                        , value = model.stretchPointer |> Maybe.withDefault 0 |> toFloat
                        , thumb = Input.defaultThumb
                        }

                _ ->
                    none

        showActionButtons =
            row [ spacing 5 ]
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

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = checkboxIcon
                , checked = model.mode == Stretch
                , label = Input.labelRight [ centerY ] (text "Stretch")
                }
    in
    -- Try with linear vector, switch to log or something else if needed.
    row [ paddingEach { edges | right = 10 }, spacing 5 ]
        [ twoWayDragControl model wrapper
        , column
            [ Element.alignLeft
            , Element.width Element.fill
            , spacing 5
            ]
            [ markerTextHelper track
            , showModeSelection
            , showSliderInStretchMode
            , showActionButtons
            ]
        , heightSlider
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


buildActions : Model -> Track -> UndoEntry
buildActions options track =
    let
        markerPosition =
            track.markedNode |> Maybe.withDefault track.currentNode

        ( from, to ) =
            ( min track.currentNode.index markerPosition.index
            , max track.currentNode.index markerPosition.index
            )

        ( beforeEnd, suffix ) =
            track.trackPoints |> List.Extra.splitAt (to + 1)

        ( prefix, region ) =
            beforeEnd |> List.Extra.splitAt from

        stretchPoint =
            case options.stretchPointer of
                Just s ->
                    track.trackPoints |> List.Extra.getAt s

                Nothing ->
                    Nothing

        newPoints =
            case ( options.mode, stretchPoint ) of
                ( Stretch, Just stretcher ) ->
                    -- Avoid potential division by zero.
                    if from < stretcher.index && stretcher.index < to then
                        stretchPoints options stretcher region

                    else
                        region |> List.map .xyz

                ( _, _ ) ->
                    movePoints options region

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { regionStart = from
            , regionEnd = to
            , originalPoints = List.map .xyz region
            , revisedPoints = newPoints
            , originalProfileXZ = List.map .profileXZ region
            }
    in
    { label = "Move & Stretch"
    , editFunction = apply undoRedoInfo
    , undoFunction = undo undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    }


apply : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
apply undoRedoInfo track =
    let
        ( beforeEnd, suffix ) =
            track.trackPoints |> List.Extra.splitAt (undoRedoInfo.regionEnd + 1)

        ( prefix, region ) =
            beforeEnd |> List.Extra.splitAt undoRedoInfo.regionStart

        translatePoint tp original revised =
            -- To make the profile come out right is more sutle.
            let
                translation =
                    Vector3d.from original revised

                verticalVector =
                    Vector3d.xyz Quantity.zero Quantity.zero (Vector3d.zComponent translation)

                newProfileXZ =
                    tp.profileXZ |> Point3d.translateBy verticalVector
            in
            { tp | xyz = revised, profileXZ = newProfileXZ }

        newPoints =
            List.map3 translatePoint
                region
                undoRedoInfo.originalPoints
                undoRedoInfo.revisedPoints
    in
    ( prefix, newPoints, suffix )


undo : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
undo undoRedoInfo track =
    let
        ( beforeEnd, suffix ) =
            track.trackPoints |> List.Extra.splitAt (undoRedoInfo.regionEnd + 1)

        ( prefix, region ) =
            beforeEnd |> List.Extra.splitAt undoRedoInfo.regionStart

        translatePoint tp original originalProfileXZ =
            -- To make the profile come out right is more sutle.
            { tp | xyz = original, profileXZ = originalProfileXZ }

        newPoints =
            List.map3 translatePoint
                region
                undoRedoInfo.originalPoints
                undoRedoInfo.originalProfileXZ
    in
    ( prefix, newPoints, suffix )


movePoints : Model -> List TrackPoint -> List (Point3d.Point3d Length.Meters LocalCoords)
movePoints options region =
    -- This used by preview and action.
    let
        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShift =
            heightOffset options.heightSliderSetting

        translation =
            -- Negate y because SVG coordinates go downards.
            Point3d.translateBy (Vector3d.xyz xShift (Quantity.negate yShift) zShift)

        ( notLast, last ) =
            region |> List.Extra.splitAt (List.length region - 1)

        ( first, regionExcludingEnds ) =
            notLast |> List.Extra.splitAt 1
    in
    List.map .xyz first
        ++ List.map (.xyz >> translation) regionExcludingEnds
        ++ List.map .xyz last


stretchPoints : Model -> TrackPoint -> List TrackPoint -> List (Point3d.Point3d Length.Meters LocalCoords)
stretchPoints options stretcher region =
    -- This used by preview and action.
    -- Here we move points either side of the stretch marker.
    let
        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShiftMax =
            Vector3d.xyz
                Quantity.zero
                Quantity.zero
                (heightOffset options.heightSliderSetting)

        horizontalTranslation =
            -- Negate y because SVG coordinates go downards.
            Vector3d.xyz xShift (Quantity.negate yShift) (meters 0)

        ( startAnchor, endAnchor ) =
            ( region |> List.head |> Maybe.withDefault stretcher
            , region |> List.Extra.last |> Maybe.withDefault stretcher
            )

        ( firstPart, secondPart ) =
            region |> List.Extra.splitAt (stretcher.index - startAnchor.index)

        ( firstPartAxis, secondPartAxis ) =
            ( Axis3d.throughPoints startAnchor.xyz stretcher.xyz
            , Axis3d.throughPoints endAnchor.xyz stretcher.xyz
            )

        ( firstPartDistance, secondPartDistance ) =
            ( Point3d.distanceFrom startAnchor.xyz stretcher.xyz
            , Point3d.distanceFrom endAnchor.xyz stretcher.xyz
            )

        distanceAlong maybeAxis p =
            case maybeAxis of
                Just axis ->
                    p |> Point3d.signedDistanceAlong axis

                Nothing ->
                    Quantity.zero

        ( adjustedFirstPoints, adjustedSecondPoints ) =
            ( List.map
                adjustRelativeToStart
                firstPart
            , List.map
                adjustRelativeToEnd
                secondPart
            )

        adjustRelativeToStart pt =
            let
                proportion =
                    Quantity.ratio
                        (pt.xyz |> distanceAlong firstPartAxis)
                        firstPartDistance
            in
            pt.xyz
                |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)

        adjustRelativeToEnd pt =
            let
                proportion =
                    Quantity.ratio
                        (pt.xyz |> distanceAlong secondPartAxis)
                        secondPartDistance
            in
            pt.xyz
                |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)
    in
    adjustedFirstPoints ++ adjustedSecondPoints


getPreview3D : Model -> Track -> List (Entity LocalCoords)
getPreview3D options track =
    let
        undoEntry =
            buildActions options track

        ( _, region, _ ) =
            undoEntry.editFunction track

        stretchPoint =
            case options.stretchPointer of
                Just sp ->
                    makeWhiteMarker sp

                Nothing ->
                    []

        makeWhiteMarker i =
            case track.trackPoints |> List.Extra.getAt i of
                Just stretch ->
                    Utils.lollipop stretch.xyz Color.white

                Nothing ->
                    []
    in
    stretchPoint ++ previewLine Color.lightPurple region


getPreviewProfile : DisplayOptions -> Model -> Track -> List (Entity LocalCoords)
getPreviewProfile display options track =
    let
        undoEntry =
            buildActions options track

        ( _, region, _ ) =
            undoEntry.editFunction track
    in
    previewProfileLine display Color.white region


getPreviewMap : DisplayOptions -> Model -> Track -> E.Value
getPreviewMap display options track =
    {-
       To return JSON:
       { "name" : "nudge"
       , "colour" : "#FFFFFF"
       , "points" : <trackPointsToJSON ...>
       }
    -}
    let
        undoEntry =
            buildActions options track

        ( _, region, _ ) =
            undoEntry.editFunction track

        fakeTrack =
            -- Just for the JSON
            { track | trackPoints = region }
    in
    E.object
        [ ( "name", E.string "stretch" )
        , ( "colour", E.string "#800080" )
        , ( "points", Track.trackToJSON fakeTrack )
        ]
