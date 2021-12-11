module CurveFormer exposing (..)

import Angle
import Arc2d
import Arc3d
import Axis2d
import Circle3d
import Color
import ColourPalette exposing (warningColor)
import Direction2d
import Direction3d
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import Geometry101
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Json.Encode as E
import Length exposing (meters)
import LineSegment2d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Point2d
import Point3d
import Polyline2d
import Polyline3d
import PostUpdateActions exposing (EditResult, UndoEntry, defaultEditResult)
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SceneBuilder exposing (highlightPoints)
import SceneBuilderProfile exposing (previewProfileLine)
import SketchPlane3d
import SpatialIndex
import Svg
import Svg.Attributes as SA
import SweptAngle
import Track exposing (Track)
import TrackEditType
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Utils exposing (flatBox, showShortMeasure, useIcon)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


type GradientSmoothing
    = Piecewise
    | Holistic


type alias Model =
    -- Circle centre is Orange marker xy translated by the vector.
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , referencePoint : Maybe TrackPoint
    , dragging : Maybe Point
    , smoothGradient : GradientSmoothing
    , pushRadius : Length.Length
    , pullDiscWidth : Length.Length
    , spacing : Length.Length
    , usePullRadius : Bool
    , pointsWithinCircle : List TrackPoint
    , pointsWithinDisc : List TrackPoint
    , circle : Maybe (Circle3d.Circle3d Length.Meters LocalCoords)
    , pointsAreContiguous : Bool
    , newTrackPoints : List (Point3d.Point3d Length.Meters LocalCoords)
    , fixedAttachmentPoints : Maybe ( Int, Int )
    , transitionRadius : Length.Length
    , lastVector : Vector2d.Vector2d Length.Meters LocalCoords
    }


controlSvgRadius =
    100


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


defaultModel : Model
defaultModel =
    { vector = Vector2d.zero
    , referencePoint = Nothing
    , dragging = Nothing
    , smoothGradient = Holistic
    , pushRadius = Length.meters 10.0
    , pullDiscWidth = Length.meters 5.0
    , spacing = Length.meters 5.0
    , usePullRadius = False
    , pointsWithinCircle = []
    , pointsWithinDisc = []
    , circle = Nothing
    , pointsAreContiguous = False
    , newTrackPoints = []
    , fixedAttachmentPoints = Nothing
    , transitionRadius = Length.meters 20.0
    , lastVector = Vector2d.zero
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
    | SetTransitionRadius Float
    | SetSpacing Float
    | ToggleUsePullRadius Bool


type alias UndoRedoInfo =
    { newNodes : List (Point3d.Point3d Length.Meters LocalCoords)
    , oldNodes : List (Point3d.Point3d Length.Meters LocalCoords)
    , attachmentPoints : Maybe ( Int, Int )
    }


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
                    , PostUpdateActions.ActionNoOp
                    )

                Just dragStart ->
                    let
                        newVector =
                            model.lastVector |> Vector2d.plus (Vector2d.from dragStart offset)
                    in
                    ( { model
                        | vector = newVector
                        , referencePoint =
                            if model.referencePoint == Nothing then
                                Just track.currentNode

                            else
                                model.referencePoint
                      }
                        |> seekNewCurve track
                    , PostUpdateActions.ActionPreview
                    )

        DraggerRelease _ ->
            ( { model
                | dragging = Nothing
                , lastVector = model.vector
              }
                |> seekNewCurve track
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
                |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        ToggleUsePullRadius bool ->
            ( { model | usePullRadius = not model.usePullRadius }
                |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        DraggerReset ->
            ( { model
                | dragging = Nothing
                , referencePoint = Nothing
                , lastVector = Vector2d.zero
                , vector = Vector2d.zero
              }
                |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        DraggerApply ->
            ( defaultModel
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesNodePosition
                (buildEditUndoActions model track)
            )

        SetPushRadius x ->
            ( { model | pushRadius = toLength x } |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        SetPullRadius x ->
            ( { model | pullDiscWidth = toLength x } |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        SetTransitionRadius x ->
            ( { model | transitionRadius = toLength x } |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )

        SetSpacing x ->
            ( { model | spacing = toLength x } |> seekNewCurve track
            , PostUpdateActions.ActionPreview
            )


buildEditUndoActions : Model -> Track -> UndoEntry
buildEditUndoActions options track =
    -- This is the +/-ve delta for possible redo. We do not include track in the closure!
    let
        updated =
            seekNewCurve track options
    in
    case updated.fixedAttachmentPoints of
        Just ( start, end ) ->
            let
                undoRedoInfo : UndoRedoInfo
                undoRedoInfo =
                    { newNodes = updated.newTrackPoints
                    , oldNodes =
                        track.trackPoints
                            |> List.drop start
                            |> List.take (end - start + 1)
                            |> List.map .xyz
                    , attachmentPoints = updated.fixedAttachmentPoints
                    }
            in
            { label = makeUndoMessage options
            , editFunction = apply undoRedoInfo
            , undoFunction = undo undoRedoInfo
            , newOrange = track.currentNode.index
            , newPurple = Maybe.map .index track.markedNode
            , oldOrange = track.currentNode.index
            , oldPurple = Maybe.map .index track.markedNode
            }

        Nothing ->
            { label = "Something amiss"
            , editFunction = always defaultEditResult
            , undoFunction = always defaultEditResult
            , newOrange = track.currentNode.index
            , newPurple = Maybe.map .index track.markedNode
            , oldOrange = track.currentNode.index
            , oldPurple = Maybe.map .index track.markedNode
            }


view : Bool -> Model -> (Msg -> msg) -> Track -> Element msg
view imperial model wrapper track =
    let
        squared x =
            x * x

        showPushRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPushRadius << squared
                , label =
                    -- Note, I want non-linear slider.
                    Input.labelBelow []
                        (text <| "Bend radius " ++ showShortMeasure imperial model.pushRadius)
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = model.pushRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showTransitionRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetTransitionRadius << squared
                , label =
                    -- Note, I want non-linear slider.
                    Input.labelBelow []
                        (text <| "Joining radius " ++ showShortMeasure imperial model.transitionRadius)
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = model.transitionRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showPullRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPullRadius
                , label =
                    Input.labelBelow []
                        (text <| "Inclusion zone " ++ showShortMeasure imperial model.pullDiscWidth)
                , min = 0.0
                , max = 20.0
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
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = model.spacing |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showActionButtons =
            row [ padding 5, spacing 5, width fill ]
                [ Input.button prettyButtonStyles
                    { label = text "Reset"
                    , onPress = Just <| wrapper DraggerReset
                    }
                , case ( List.length model.newTrackPoints >= 3, model.pointsAreContiguous ) of
                    ( _, True ) ->
                        Input.button
                            prettyButtonStyles
                            { label = text "Apply"
                            , onPress = Just <| wrapper DraggerApply
                            }

                    ( _, False ) ->
                        Input.button
                            disabledButtonStyles
                            { label = paragraph [ width fill ] <| [ text "Points must be contiguous" ]
                            , onPress = Nothing
                            }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = checkboxIcon
                , checked = model.smoothGradient == Holistic
                , label = Input.labelRight [ centerY ] (text "Smooth gradient")
                }

        showPullSelection =
            Input.checkbox []
                { onChange = wrapper << ToggleUsePullRadius
                , icon = checkboxIcon
                , checked = model.usePullRadius
                , label = Input.labelRight [ centerY ] (text "Include outliers")
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
        , column [ width fill ]
            [ wrappedRow
                [ Element.alignLeft
                , Element.width Element.fill
                , spacing 5
                ]
                [ showPushRadiusSlider
                , showTransitionRadiusSlider
                , showSpacingSlider
                , showPullSelection
                , if model.usePullRadius then
                    showPullRadiusSlider

                  else
                    none
                ]
            , showModeSelection
            , showActionButtons
            ]
        ]


toolLabel =
    "Curve Former"


info : String
info =
    """## Curve Former

It's an alternative Bend Smoother. You set the bend radius.

Position the Orange marker on the track, near a bend that you wish to shape.
Use the circular drag control to position the white circle over the desired centre of the bend.
Set the desired bend radius. You can then move the Orange pointer separately; use Reset
to snap the circle back to the Orange marker.

If you want outlying points to be "pulled inwards", select _Attract Outliers_ and adjust the
extra slider.

If you include points from another section of the track, we get all confused. You may be
able to resolve this by additionally using the Orange and Purple markers to clarify which section
of track you're editing.

The tool will seek a smooth transition to track outside the circle, using a counter-directional
arc of the same radius. This may extend beyond the marked points. The preview reflects
what the Apply button will do.

You can choose to have a uniform gradient is applied along the new region of track, or
to derive elevations by interpolation of the original track according to distance.

Reducing spacing gives a smoother curve by adding more new trackpoints.

"""


apply : UndoRedoInfo -> Track -> EditResult
apply undoRedo track =
    case undoRedo.attachmentPoints of
        Just ( from, to ) ->
            let
                ( prefix, theRest ) =
                    List.Extra.splitAt (from + 1) track.trackPoints

                ( replacedTrack, suffix ) =
                    List.Extra.splitAt (to - from - 1) theRest
            in
            { before = prefix
            , edited = List.map trackPointFromPoint undoRedo.newNodes
            , after = suffix
            , earthReferenceCoordinates = track.earthReferenceCoordinates
            , graph = track.graph
            }

        Nothing ->
            { before = []
            , edited = track.trackPoints
            , after = []
            , earthReferenceCoordinates = track.earthReferenceCoordinates
            , graph = track.graph
            }


undo : UndoRedoInfo -> Track -> EditResult
undo undoRedo track =
    case undoRedo.attachmentPoints of
        Just ( from, to ) ->
            let
                ( prefix, theRest ) =
                    List.Extra.splitAt (1 + from) track.trackPoints

                ( replacedTrack, suffix ) =
                    List.Extra.splitAt (List.length undoRedo.newNodes) theRest
            in
            { before = prefix
            , edited = List.map trackPointFromPoint undoRedo.oldNodes
            , after = suffix
            , earthReferenceCoordinates = track.earthReferenceCoordinates
            , graph = track.graph
            }

        Nothing ->
            { before = []
            , edited = track.trackPoints
            , after = []
            , earthReferenceCoordinates = track.earthReferenceCoordinates
            , graph = track.graph
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
            case model.referencePoint of
                Just localReference ->
                    localReference.xyz
                        |> Point3d.translateBy translation

                Nothing ->
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
            case model.referencePoint of
                Just localReference ->
                    localReference.xyz
                        |> Point3d.translateBy translation

                Nothing ->
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


type alias IntersectionInformation =
    { intersection : Point2d.Point2d Length.Meters LocalCoords
    , distanceAlong : Length.Length
    , tangentPoint : Point2d.Point2d Length.Meters LocalCoords
    , joinsBendAt : Point2d.Point2d Length.Meters LocalCoords
    , originalTrackPoint : TrackPoint
    }


type TransitionMode
    = EntryMode
    | ExitMode


seekNewCurve : Track -> Model -> Model
seekNewCurve track model =
    -- The compute we do here is most of the work for the Apply, so
    -- we keep it in our model.
    let
        ( orange, purple ) =
            ( track.currentNode.index
            , Maybe.map .index track.markedNode |> Maybe.withDefault track.currentNode.index
            )

        ( startRange, endRange ) =
            if track.markedNode == Nothing then
                ( 0, List.length track.trackPoints - 1 )

            else
                ( min orange purple, max orange purple )

        circle =
            getCircle model track.currentNode

        ( centre, axis, drawingPlane ) =
            -- Yes, I know this is trite.
            ( Circle3d.centerPoint circle
            , Circle3d.axis circle
            , SketchPlane3d.xy
                |> SketchPlane3d.translateBy
                    (Vector3d.withLength (Point3d.zCoordinate <| Circle3d.centerPoint circle) Direction3d.positiveZ)
            )

        centreOnPlane =
            -- Most of the work is planar; we layer the elevations on at the end.
            centre |> Point3d.projectInto drawingPlane

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
                |> List.filter (\p -> p.index >= startRange && p.index <= endRange)

        pointsWithinDisc =
            -- Look for points that could be pulled in to the bend
            let
                ( lowestInteriorIndex, highestInteriorIndex ) =
                    ( List.Extra.minimumBy .index pointsWithinCircle |> Maybe.map .index |> Maybe.withDefault 0
                    , List.Extra.maximumBy .index pointsWithinCircle |> Maybe.map .index |> Maybe.withDefault 0
                    )
            in
            SpatialIndex.queryWithFilter track.spatialIndex boundingBox isWithinDisc
                |> List.map .content

        allPoints =
            List.sortBy .index (pointsWithinCircle ++ pointsWithinDisc)

        -- Care over turn direction. Bend may exceed 180 degrees and Angle would not be good.
        -- Perhaps simpler and more reliable is "which side of the entry road is the centre?"
        isLeftHandBend =
            -- OK, not the entry road, but the first road segment in the circle.
            -- Positive distance == LEFT OF ROAD, i.e. left hand bend.
            -- Note I ignore the obvious "on the axis" case; this may bite me later.
            -- WHY do I end up with this fuggle? What's the better practice?
            case List.head allPoints of
                Just isFirstPoint ->
                    case isFirstPoint.afterDirection of
                        Just roadDirection ->
                            let
                                roadDirectionInPlane =
                                    Direction3d.projectInto drawingPlane roadDirection
                            in
                            case roadDirectionInPlane of
                                Just roadPlanarDirection ->
                                    let
                                        roadAxis =
                                            Axis2d.withDirection
                                                roadPlanarDirection
                                                (Point3d.projectInto drawingPlane isFirstPoint.xyz)
                                    in
                                    Point2d.signedDistanceFrom roadAxis centreOnPlane
                                        |> Quantity.greaterThanOrEqualTo Quantity.zero

                                Nothing ->
                                    True

                        Nothing ->
                            True

                Nothing ->
                    True

        findAcceptableTransition : TransitionMode -> TrackPoint -> TrackPoint -> Maybe IntersectionInformation
        findAcceptableTransition mode tp1 tp2 =
            let
                -- Construct a parallel to the given road segment, 'r' meters towards the arc start.
                -- Where this intersects the '2r' circle, is centre of the entry bend.
                -- Where this new circle is tangent to the line segment is where the entry begins.
                -- If the entry precedes the segment start, we have failed to find a line;
                -- the user will need to move the marker. Or we recurse back down the track; that might work.
                entryLineSegment =
                    LineSegment2d.from
                        (Point3d.projectInto drawingPlane tp1.xyz)
                        (Point3d.projectInto drawingPlane tp2.xyz)

                entryLineAxis =
                    Axis2d.throughPoints
                        (LineSegment2d.startPoint entryLineSegment)
                        (LineSegment2d.endPoint entryLineSegment)

                entryLineShiftVector =
                    let
                        shiftAmount =
                            if isLeftHandBend then
                                Quantity.negate model.transitionRadius

                            else
                                model.transitionRadius
                    in
                    Maybe.map (Vector2d.withLength shiftAmount)
                        (LineSegment2d.perpendicularDirection entryLineSegment)

                shiftedEntryLine =
                    case entryLineShiftVector of
                        Just theVector ->
                            Just <| LineSegment2d.translateBy theVector entryLineSegment

                        _ ->
                            Nothing

                outerCircleIntersections =
                    case shiftedEntryLine of
                        Just line ->
                            let
                                lineEqn =
                                    Geometry101.lineEquationFromTwoPoints
                                        (LineSegment2d.startPoint line |> Point2d.toRecord Length.inMeters)
                                        (LineSegment2d.endPoint line |> Point2d.toRecord Length.inMeters)

                                outerCircle =
                                    { centre = centreOnPlane |> Point2d.toRecord Length.inMeters
                                    , radius =
                                        Quantity.plus model.pushRadius model.transitionRadius
                                            |> Length.inMeters
                                    }
                            in
                            Geometry101.lineCircleIntersections lineEqn outerCircle
                                |> List.map (Point2d.fromRecord Length.meters)

                        Nothing ->
                            []

                validCounterBendCentresAndTangentPoints : List IntersectionInformation
                validCounterBendCentresAndTangentPoints =
                    -- Point is 'valid' if it is not 'before' the segment start (or axis origin).
                    -- Want to return the outer intersection point (new bend centre) and the tangent point.
                    case entryLineAxis of
                        Just sameOldAxis ->
                            let
                                selectionFunction =
                                    case mode of
                                        EntryMode ->
                                            List.Extra.minimumBy (.distanceAlong >> Length.inMeters)

                                        ExitMode ->
                                            List.Extra.maximumBy (.distanceAlong >> Length.inMeters)

                                elaborateIntersectionPoint i =
                                    let
                                        distanceAlong =
                                            Point2d.signedDistanceAlong sameOldAxis i

                                        tangentPoint2d =
                                            Point2d.along sameOldAxis distanceAlong

                                        bendJoinPoint =
                                            Point2d.interpolateFrom
                                                centreOnPlane
                                                i
                                                (Quantity.ratio model.pushRadius
                                                    (Quantity.plus model.pushRadius model.transitionRadius)
                                                )
                                    in
                                    { intersection = i
                                    , distanceAlong = distanceAlong
                                    , tangentPoint = tangentPoint2d
                                    , joinsBendAt = bendJoinPoint
                                    , originalTrackPoint =
                                        -- Point on track, used to compute altitudes and
                                        -- final line segment to adjoining tangent point.
                                        case mode of
                                            EntryMode ->
                                                tp1

                                            ExitMode ->
                                                tp2
                                    }
                            in
                            List.map elaborateIntersectionPoint outerCircleIntersections
                                |> selectionFunction
                                |> Maybe.Extra.toList
                                |> List.filter (.distanceAlong >> Quantity.greaterThanOrEqualTo Quantity.zero)
                                |> List.filter (.distanceAlong >> Quantity.lessThanOrEqualTo (LineSegment2d.length entryLineSegment))

                        _ ->
                            []
            in
            case validCounterBendCentresAndTangentPoints of
                [] ->
                    Nothing

                bestFound :: _ ->
                    Just bestFound

        arcToSegments arc =
            let
                arcLength =
                    Length.inMeters (Arc2d.radius arc)
                        * Angle.inRadians (Arc2d.sweptAngle arc)
                        |> abs

                entryArcNumSegments =
                    arcLength
                        / Length.inMeters model.spacing
                        |> ceiling
                        |> max 1
            in
            Arc2d.segments entryArcNumSegments arc |> Polyline2d.segments

        entryCurveSeeker : Int -> Maybe IntersectionInformation
        entryCurveSeeker index =
            let
                pointsDefiningEntryLine =
                    -- We know these points exist but, you know, type safety.
                    ( List.Extra.getAt (index - 1) track.trackPoints
                    , List.Extra.getAt index track.trackPoints
                    )
            in
            case pointsDefiningEntryLine of
                ( Just tp1, Just tp2 ) ->
                    case findAcceptableTransition EntryMode tp1 tp2 of
                        Just transition ->
                            Just transition

                        Nothing ->
                            if index > 1 && index > tp1.index - 5 then
                                entryCurveSeeker (index - 1)

                            else
                                Nothing

                _ ->
                    Nothing

        exitCurveSeeker : Int -> Int -> Maybe IntersectionInformation
        exitCurveSeeker routeLength index =
            let
                pointsDefiningExitLine =
                    -- We know these points exist but, you know, type safety.
                    ( List.Extra.getAt index track.trackPoints
                    , List.Extra.getAt (index + 1) track.trackPoints
                    )
            in
            case pointsDefiningExitLine of
                ( Just tp1, Just tp2 ) ->
                    case findAcceptableTransition ExitMode tp1 tp2 of
                        Just transition ->
                            Just transition

                        Nothing ->
                            if index < routeLength - 2 && index < tp2.index + 5 then
                                exitCurveSeeker routeLength (index + 1)

                            else
                                Nothing

                _ ->
                    Nothing

        ( entryInformation, exitInformation ) =
            let
                routeLength =
                    List.length track.trackPoints
            in
            ( Maybe.andThen (.index >> entryCurveSeeker) (List.head allPoints)
            , Maybe.andThen (.index >> exitCurveSeeker routeLength) (List.Extra.last allPoints)
            )

        entryCurve =
            case entryInformation of
                Just { intersection, distanceAlong, tangentPoint, joinsBendAt } ->
                    Arc2d.withRadius
                        model.transitionRadius
                        (if isLeftHandBend then
                            SweptAngle.smallNegative

                         else
                            SweptAngle.smallPositive
                        )
                        tangentPoint
                        joinsBendAt
                        |> Maybe.map arcToSegments
                        |> Maybe.withDefault []

                Nothing ->
                    []

        exitCurve =
            case exitInformation of
                Just { intersection, distanceAlong, tangentPoint, joinsBendAt } ->
                    Arc2d.withRadius
                        model.transitionRadius
                        (if isLeftHandBend then
                            SweptAngle.smallNegative

                         else
                            SweptAngle.smallPositive
                        )
                        joinsBendAt
                        tangentPoint
                        |> Maybe.map arcToSegments
                        |> Maybe.withDefault []

                Nothing ->
                    []

        theArcItself =
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    let
                        ( entryDirection, exitDirection ) =
                            ( Direction2d.from centreOnPlane entry.joinsBendAt
                            , Direction2d.from centreOnPlane exit.joinsBendAt
                            )

                        turn =
                            Maybe.map2 Direction2d.angleFrom entryDirection exitDirection
                    in
                    case turn of
                        Just turnAngle ->
                            Arc2d.withRadius
                                model.pushRadius
                                (if isLeftHandBend then
                                    if turnAngle |> Quantity.greaterThanOrEqualTo Quantity.zero then
                                        SweptAngle.smallPositive

                                    else
                                        SweptAngle.largePositive

                                 else if turnAngle |> Quantity.lessThanOrEqualTo Quantity.zero then
                                    SweptAngle.smallNegative

                                 else
                                    SweptAngle.largeNegative
                                )
                                entry.joinsBendAt
                                exit.joinsBendAt
                                |> Maybe.map arcToSegments
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                _ ->
                    []

        prepareOriginalAltitudesForInterpolation =
            case attachmentPoints of
                Just ( start, end ) ->
                    let
                        originalSection =
                            track.trackPoints |> List.take (end + 1) |> List.drop start

                        startDistance =
                            List.head originalSection
                                |> Maybe.map .distanceFromStart
                                |> Maybe.withDefault Quantity.zero

                        endDistance =
                            List.Extra.last originalSection
                                |> Maybe.map .distanceFromStart
                                |> Maybe.withDefault Quantity.positiveInfinity

                        length =
                            endDistance |> Quantity.minus startDistance

                        altitudesByFraction =
                            originalSection
                                |> List.map
                                    (\pt ->
                                        ( Quantity.ratio (pt.distanceFromStart |> Quantity.minus startDistance)
                                            length
                                        , Point3d.zCoordinate pt.xyz
                                        )
                                    )
                    in
                    altitudesByFraction

                Nothing ->
                    []

        interpolateOriginalAltitudesByDistance fraction =
            let
                twoSides =
                    prepareOriginalAltitudesForInterpolation
                        |> List.Extra.splitWhen (\( k, _ ) -> k >= fraction)
            in
            case twoSides of
                Just ( beforePairs, afterPairs ) ->
                    let
                        ( lastBefore, firstAfter ) =
                            ( List.Extra.last beforePairs, List.head afterPairs )
                    in
                    case ( lastBefore, firstAfter ) of
                        ( Just ( priorFraction, priorAltitude ), Just ( nextFraction, nextAltitude ) ) ->
                            let
                                ( beforeContribution, afterContribution ) =
                                    ( (nextFraction - fraction) / (nextFraction - priorFraction)
                                    , (fraction - priorFraction) / (nextFraction - priorFraction)
                                    )
                            in
                            Quantity.plus
                                (Quantity.multiplyBy beforeContribution priorAltitude)
                                (Quantity.multiplyBy afterContribution nextAltitude)

                        ( Just ( priorFraction, priorAltitude ), Nothing ) ->
                            -- Might happen at the end.
                            priorAltitude

                        ( Nothing, Just ( nextFraction, nextAltitude ) ) ->
                            -- Probably should not happen, but
                            nextAltitude

                        ( Nothing, Nothing ) ->
                            -- Huh?
                            Quantity.zero

                Nothing ->
                    Quantity.zero

        -- Given we interpolate using interval [0..1], we can figure out the
        -- original altitude by interpolation using this as a proportion of
        -- distance along the original section of track.
        newBendEntirely =
            -- We (probably) have a bend, but it's flat. We need to interpolate altitudes.
            -- We can do this uniformly, or based on the original profile (by distance portion).
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    let
                        completeSegments =
                            [ LineSegment2d.from
                                (Point3d.projectInto drawingPlane entry.originalTrackPoint.xyz)
                                entry.tangentPoint
                            ]
                                ++ entryCurve
                                ++ theArcItself
                                ++ exitCurve
                                ++ [ LineSegment2d.from
                                        exit.tangentPoint
                                        (Point3d.projectInto drawingPlane exit.originalTrackPoint.xyz)
                                   ]

                        actualNewLength =
                            completeSegments |> List.map LineSegment2d.length |> Quantity.sum

                        altitudeChange =
                            Quantity.minus
                                (Point3d.zCoordinate entry.originalTrackPoint.xyz)
                                (Point3d.zCoordinate exit.originalTrackPoint.xyz)

                        cumulativeDistances =
                            completeSegments
                                |> List.Extra.scanl
                                    (\seg run -> Quantity.plus run (LineSegment2d.length seg))
                                    Quantity.zero

                        adjustedAltitudes =
                            -- Can make this return the altitude adjusted end track point.
                            List.map2
                                (\seg dist ->
                                    let
                                        originalSegmentStart =
                                            LineSegment2d.startPoint seg

                                        proportionalDistance =
                                            Quantity.ratio dist actualNewLength

                                        adjustment =
                                            altitudeChange |> Quantity.multiplyBy proportionalDistance

                                        newAltitude =
                                            case model.smoothGradient of
                                                Holistic ->
                                                    Point3d.zCoordinate entry.originalTrackPoint.xyz
                                                        |> Quantity.plus adjustment

                                                Piecewise ->
                                                    interpolateOriginalAltitudesByDistance proportionalDistance
                                    in
                                    Point3d.xyz
                                        (Point2d.xCoordinate originalSegmentStart)
                                        (Point2d.yCoordinate originalSegmentStart)
                                        newAltitude
                                )
                                (List.drop 0 completeSegments)
                                cumulativeDistances
                    in
                    adjustedAltitudes

                _ ->
                    []

        attachmentPoints =
            -- We need this to inform "Main" of where the track has changed.
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    Just
                        ( entry.originalTrackPoint.index
                        , exit.originalTrackPoint.index
                        )

                _ ->
                    Nothing
    in
    { model
        | pointsWithinCircle = pointsWithinCircle
        , pointsWithinDisc = pointsWithinDisc
        , circle = Just circle
        , pointsAreContiguous = areContiguous allPoints
        , newTrackPoints = newBendEntirely
        , fixedAttachmentPoints = attachmentPoints
    }


getPreview3D : Model -> Track -> List (Entity LocalCoords)
getPreview3D model track =
    showCircle model
        ++ showDisc model
        ++ highlightPoints Color.white model.pointsWithinCircle
        ++ highlightPoints Color.blue model.pointsWithinDisc
        ++ (highlightPoints Color.lightYellow <| List.map trackPointFromPoint model.newTrackPoints)


getPreviewProfile : DisplayOptions -> Model -> Track -> List (Entity LocalCoords)
getPreviewProfile display model track =
    previewProfileLine display Color.lightYellow <| List.map trackPointFromPoint model.newTrackPoints


getPreviewMap : DisplayOptions -> Model -> Track -> E.Value
getPreviewMap _ model track =
    {-
       To return JSON:
       { "name" : "nudge"
       , "colour" : "#FFFFFF"
       , "points" : <trackPointsToJSON ...>
       }
    -}
    let
        refreshModel =
            -- Becasue tab may be opened or closed.
            seekNewCurve track model

        fakeTrack =
            -- Just for the JSON
            { track | trackPoints = List.map trackPointFromPoint refreshModel.newTrackPoints }
    in
    E.object
        [ ( "name", E.string "curve" )
        , ( "colour", E.string "#FFFF00" )
        , ( "points", Track.trackToJSON fakeTrack )
        ]


makeUndoMessage : Model -> String
makeUndoMessage model =
    "Curve Former"


areContiguous : List TrackPoint -> Bool
areContiguous points =
    let
        indices =
            List.map .index points
    in
    (List.length points == 0)
        || (case ( List.maximum indices, List.minimum indices ) of
                ( Just isMax, Just isMin ) ->
                    (isMax - isMin == List.length points - 1) && List.Extra.allDifferent indices

                _ ->
                    False
           )
