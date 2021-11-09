module CurveFormer exposing (..)

import Angle
import Arc3d
import Axis3d
import Circle3d
import Color
import ColourPalette exposing (warningColor)
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels
import Point2d
import Point3d
import Polyline3d
import PostUpdateActions
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SpatialIndex
import Svg
import Svg.Attributes as SA
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (flatBox, showShortMeasure, useIcon)
import Vector2d
import Vector3d
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, edges, prettyButtonStyles)


type GradientSmoothing
    = Piecewise
    | Holistic


type alias Model =
    -- Circle centre is Orange marker xy translated by the vector.
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    , smoothGradient : GradientSmoothing
    , pushRadius : Length.Length
    , pullDiscWidth : Length.Length
    , spacing : Length.Length
    , usePullRadius : Bool
    , pointsWithinCircle : List TrackPoint
    , pointsWithinDisc : List TrackPoint
    , circle : Maybe (Circle3d.Circle3d Length.Meters LocalCoords)
    , areContiguous : Bool
    , newTrackPoints : List TrackPoint
    }


controlSvgRadius =
    100


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


defaultModel : Model
defaultModel =
    { vector = Vector2d.zero
    , dragging = Nothing
    , smoothGradient = Holistic
    , pushRadius = Length.meters 10.0
    , pullDiscWidth = Length.meters 5.0
    , spacing = Length.meters 5.0
    , usePullRadius = False
    , pointsWithinCircle = []
    , pointsWithinDisc = []
    , circle = Nothing
    , areContiguous = False
    , newTrackPoints = []
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool
    | DraggerReset
    | DraggerApply
    | SetPushRadius Float
    | SetPullRadius Float
    | SetSpacing Float
    | ToggleUsePullRadius Bool


toLength : Float -> Length.Length
toLength sliderValue =
    Length.meters sliderValue


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


settingNotZero : Model -> Bool
settingNotZero model =
    Vector2d.direction model.vector
        /= Nothing
        || (model.pushRadius |> Quantity.greaterThan Quantity.zero)


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
            , SA.r <| String.fromInt controlSvgRadius
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
                | smoothGradient =
                    case model.smoothGradient of
                        Piecewise ->
                            Holistic

                        Holistic ->
                            Piecewise
              }
            , PostUpdateActions.ActionPreview
            )

        ToggleUsePullRadius bool ->
            ( { model | usePullRadius = not model.usePullRadius }
            , PostUpdateActions.ActionPreview
            )

        DraggerReset ->
            ( defaultModel
            , PostUpdateActions.ActionPreview
            )

        DraggerApply ->
            ( model
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (apply track model)
                (makeUndoMessage model)
            )

        SetPushRadius x ->
            ( { model | pushRadius = toLength x }
            , PostUpdateActions.ActionPreview
            )

        SetPullRadius x ->
            ( { model | pullDiscWidth = toLength x }
            , PostUpdateActions.ActionPreview
            )

        SetSpacing x ->
            ( { model | spacing = toLength x }
            , PostUpdateActions.ActionPreview
            )


view : Bool -> Model -> (Msg -> msg) -> Track -> Element msg
view imperial model wrapper track =
    let
        showPushRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPushRadius
                , label =
                    Input.labelBelow []
                        (text <| "Radius " ++ showShortMeasure imperial model.pushRadius)
                , min = 2.0
                , max = 100.0
                , step = Nothing
                , value = model.pushRadius |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showPullRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPullRadius
                , label =
                    Input.labelBelow []
                        (text <| "Inclusion zone " ++ showShortMeasure imperial model.pullDiscWidth)
                , min = 0.0
                , max = 50.0
                , step = Nothing
                , value = model.pullDiscWidth |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showSpacingSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetSpacing
                , label =
                    Input.labelBelow []
                        (text <| "Spacing " ++ showShortMeasure imperial model.spacing)
                , min = 1.0
                , max = 10.0
                , step = Nothing
                , value = model.spacing |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showActionButtons =
            row [ spacing 5 ]
                [ Input.button prettyButtonStyles
                    { label = text "Reset", onPress = Just <| wrapper DraggerReset }
                , Input.button
                    prettyButtonStyles
                    { label = text "Apply"
                    , onPress = Just <| wrapper DraggerApply
                    }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = checkboxIcon
                , checked = model.smoothGradient == Piecewise
                , label = Input.labelRight [ centerY ] (text "Preserve elevations")
                }

        showPullSelection =
            Input.checkbox []
                { onChange = wrapper << ToggleUsePullRadius
                , icon = checkboxIcon
                , checked = model.usePullRadius
                , label = Input.labelRight [ centerY ] (text "Attract outliers")
                }

        showHelpfulMessage =
            row [ padding 5, spacing 10, Background.color warningColor, width fill ]
                [ useIcon FeatherIcons.info
                , paragraph [] <| [ text "I don't know what to do without contiguous points." ]
                ]
    in
    -- Try with linear vector, switch to log or something else if needed.
    row [ paddingEach { edges | right = 10 }, spacing 5 ]
        [ twoWayDragControl model wrapper
        , column
            [ Element.alignLeft
            , Element.width Element.fill
            , spacing 5
            ]
            [ showModeSelection
            , showPullSelection
            , showPushRadiusSlider
            , if model.usePullRadius then
                showPullRadiusSlider

              else
                none
            , showSpacingSlider
            , if model.areContiguous then
                showActionButtons

              else
                showHelpfulMessage
            ]
        ]


toolLabel =
    "Curve Former"


info : String
info =
    """## Curve Former

It's the new Bend Smoother. It forces points within range onto the white circle.

Position the Orange marker on the track, on a bend that you wish to shape.
Use the circular drag control to position the white circle over the desired centre of the bend.
Set the desired bend radius.

Contiguous points inside the circle will be moved radially to the circle.

If you also deploy the Purple marker, then any points between the markers that lay outside the circle,
but which lay between interior points as seen along the track, will be moved onto the circle.

The tool will seek a smooth transition to track outside the circle, using a counter-directional
arc of the same radius if necessary. This may extend beyond the marked points. The preview reflects
what the Apply button will do.

With Preserve Elevations, it will retain existing elevations and interpolate piecewise between them.
Without, it will provide a smooth gradient over the entire new bend.

Less spacing gives a smoother curve by adding more new trackpoints.

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
            case model.smoothGradient of
                Piecewise ->
                    usePiecewiseGradientSmoothing model track

                Holistic ->
                    useHolisticGradientSmoothing model track

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


getCircle : Model -> TrackPoint -> Circle3d.Circle3d Length.Meters LocalCoords
getCircle model orange =
    let
        translation =
            -- Flip Y because drag control is SVG coordinate based.
            Vector3d.xyz (Vector2d.xComponent model.vector)
                (Vector2d.yComponent model.vector |> Quantity.negate)
                Quantity.zero

        centre =
            orange.xyz
                |> Point3d.translateBy translation
    in
    Circle3d.withRadius model.pushRadius Direction3d.positiveZ centre


getOuterCircle : Model -> TrackPoint -> Circle3d.Circle3d Length.Meters LocalCoords
getOuterCircle model orange =
    let
        translation =
            -- Flip Y because drag control is SVG coordinate based.
            Vector3d.xyz (Vector2d.xComponent model.vector)
                (Vector2d.yComponent model.vector |> Quantity.negate)
                Quantity.zero

        centre =
            orange.xyz
                |> Point3d.translateBy translation

        outerRadius =
            model.pushRadius |> Quantity.plus model.pullDiscWidth
    in
    Circle3d.withRadius outerRadius Direction3d.positiveZ centre


showCircle : Model -> List (Entity LocalCoords)
showCircle model =
    case model.circle of
        Just circle ->
            let
                arc =
                    Circle3d.toArc circle

                segments =
                    Arc3d.segments 20 arc |> Polyline3d.segments

                material =
                    Material.color Color.white

                drawSegment segment =
                    Scene3d.lineSegment material segment
            in
            List.map drawSegment segments

        Nothing ->
            []


showDisc : Model -> List (Entity LocalCoords)
showDisc model =
    case model.circle of
        Just innerCircle ->
            let
                centre =
                    Circle3d.centerPoint innerCircle

                direction =
                    Circle3d.axialDirection innerCircle

                outerCircle =
                    Circle3d.withRadius
                        (model.pushRadius |> Quantity.plus model.pullDiscWidth)
                        direction
                        centre

                arc =
                    Circle3d.toArc outerCircle

                segments =
                    Arc3d.segments 20 arc |> Polyline3d.segments

                material =
                    Material.color Color.lightYellow

                drawSegment segment =
                    Scene3d.lineSegment material segment
            in
            if model.usePullRadius then
                List.map drawSegment segments

            else
                []

        Nothing ->
            []


highlightPoints : Color.Color -> List TrackPoint -> List (Entity LocalCoords)
highlightPoints color points =
    let
        material =
            Material.color color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 5 } material p.xyz
    in
    List.map highlightPoint points


preview : Model -> Track -> Model
preview model track =
    -- The compute we do here is most of the work for the Apply, so
    -- we keep it in our model.
    let
        circle =
            getCircle model track.currentNode

        ( centre, axis ) =
            ( Circle3d.centerPoint circle
            , Circle3d.axis circle
            )

        isWithinCircle pt =
            Point3d.distanceFromAxis axis pt.xyz
                |> Quantity.lessThanOrEqualTo model.pushRadius

        isWithinDisc pt =
            let
                distance =
                    Point3d.distanceFromAxis axis pt.xyz

                outerRadius =
                    model.pushRadius |> Quantity.plus model.pullDiscWidth
            in
            model.usePullRadius
                && Quantity.greaterThan model.pushRadius distance
                && Quantity.lessThanOrEqualTo outerRadius distance

        boundingBox =
            flatBox <| Circle3d.boundingBox circle

        pointsWithinCircle =
            SpatialIndex.queryWithFilter track.spatialIndex boundingBox isWithinCircle
                |> List.map .content

        pointsWithinDisc =
            SpatialIndex.queryWithFilter track.spatialIndex boundingBox isWithinDisc
                |> List.map .content

        allPoints =
            pointsWithinCircle ++ pointsWithinDisc

        ( from, to ) =
            let
                allIndices =
                    List.map .index allPoints
            in
            ( List.maximum allIndices |> Maybe.withDefault 0
            , List.minimum allIndices |> Maybe.withDefault 0
            )

        previewTrackPoints =
            -- Let's ignore elevation initially.
            -- Let's make this just the relocated points, now on the inner circle.
            allPoints |> List.map moveToInnerCircle

        moveToInnerCircle : TrackPoint -> TrackPoint
        moveToInnerCircle pt =
            let
                verticalOffset =
                    Point3d.signedDistanceAlong axis pt.xyz

                verticalVector =
                    Vector3d.withLength verticalOffset Direction3d.positiveZ

                scaleAboutPoint =
                    Point3d.translateBy verticalVector centre

                currentVector =
                    Vector3d.from scaleAboutPoint pt.xyz

                newVector =
                    Vector3d.scaleTo model.pushRadius currentVector
            in
            { pt | xyz = Point3d.translateBy newVector centre }

        ( trackBeforePreviewEnd, trackAfterPreviewEnd ) =
            List.Extra.splitAt (to + 2) previewTrackPoints

        ( trackBeforePreviewStart, previewZone ) =
            List.Extra.splitAt (from - 1) trackBeforePreviewEnd
    in
    { model
        | pointsWithinCircle = pointsWithinCircle
        , pointsWithinDisc = pointsWithinDisc
        , circle = Just circle
        , areContiguous = areContiguous (model.pointsWithinDisc ++ model.pointsWithinCircle)
        , newTrackPoints = previewTrackPoints
    }


getPreview : Model -> List (Entity LocalCoords)
getPreview model =
    showCircle model
        ++ showDisc model
        ++ highlightPoints Color.white model.pointsWithinCircle
        ++ highlightPoints Color.yellow model.pointsWithinDisc
        ++ highlightPoints Color.lightBlue model.newTrackPoints


usePiecewiseGradientSmoothing : Model -> Track -> List TrackPoint
usePiecewiseGradientSmoothing model track =
    track.trackPoints


useHolisticGradientSmoothing : Model -> Track -> List TrackPoint
useHolisticGradientSmoothing model track =
    track.trackPoints


makeUndoMessage : Model -> String
makeUndoMessage model =
    "Curve Former"


areContiguous : List TrackPoint -> Bool
areContiguous points =
    let
        indices =
            List.map .index points
    in
    case ( List.maximum indices, List.minimum indices ) of
        ( Just isMax, Just isMin ) ->
            (isMax - isMin == List.length points - 1) && List.Extra.allDifferent indices

        _ ->
            False
