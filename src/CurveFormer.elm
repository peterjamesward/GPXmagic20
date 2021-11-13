module CurveFormer exposing (..)

import Angle
import Arc2d
import Arc3d
import Axis2d
import Axis3d
import Circle3d
import Color
import ColourPalette exposing (warningColor)
import Dict exposing (Dict)
import Direction2d
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import Geometry101
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Pixels
import Plane3d
import Point2d
import Point3d
import Polyline3d
import PostUpdateActions
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import SpatialIndex
import Svg
import Svg.Attributes as SA
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
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
    , newTrackPoints : List TrackPoint
    , fixedAttachmentPoints : Maybe ( Int, Int )
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
                        , referencePoint =
                            if model.referencePoint == Nothing then
                                Just track.currentNode

                            else
                                model.referencePoint
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
                PostUpdateActions.EditPreservesNodePosition
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
            row [ padding 5, spacing 5, width fill ]
                [ Input.button prettyButtonStyles
                    { label = text "Reset", onPress = Just <| wrapper DraggerReset }
                , case ( List.length model.newTrackPoints >= 3, model.pointsAreContiguous ) of
                    ( True, True ) ->
                        Input.button
                            prettyButtonStyles
                            { label = text "Apply"
                            , onPress = Just <| wrapper DraggerApply
                            }

                    ( True, False ) ->
                        Input.button
                            disabledButtonStyles
                            { label = paragraph [ width fill ] <| [ text "Points must be contiguous" ]
                            , onPress = Nothing
                            }

                    ( False, _ ) ->
                        Input.button
                            disabledButtonStyles
                            { label = paragraph [] <| [ text "Need at least three points" ]
                            , onPress = Nothing
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
                [ showModeSelection
                , showPullSelection
                , showPushRadiusSlider
                , if model.usePullRadius then
                    showPullRadiusSlider

                  else
                    none
                , showSpacingSlider
                ]
            , showActionButtons
            ]
        ]


toolLabel =
    "Curve Former"


info : String
info =
    """## Curve Former

It's the new Bend Smoother. You can increase or decrease bend radius.

Position the Orange marker on the track, near a bend that you wish to shape.
Use the circular drag control to position the white circle over the desired centre of the bend.
Set the desired bend radius.

Contiguous points inside the circle will be moved radially to the circle.

If you want points outside to be "pulled inwards", select _Attract Outliers_ and adjust the
extra slider.

If you include points from another section of the track, we get all confused. You may be
able to resolve this by additionally using the Orange and Purple markers to clarify which section
of track you're editing.

The tool will seek a smooth transition to track outside the circle, using a counter-directional
arc of the same radius if necessary. This may extend beyond the marked points. The preview reflects
what the Apply button will do.

With Preserve Elevations, it will retain existing elevations and interpolate piecewise between them.
Otherwise it will provide a smooth gradient over the entire new bend.

Less spacing gives a smoother curve by adding more new trackpoints.

"""


apply : Track -> Model -> Track
apply track model =
    case model.fixedAttachmentPoints of
        Just ( from, to ) ->
            let
                ( beforeAndIncluding, followingTrack ) =
                    List.Extra.splitAt to track.trackPoints

                ( precedingTrack, replacedTrack ) =
                    List.Extra.splitAt (from - 1) beforeAndIncluding

                recombinedTrack =
                    precedingTrack ++ model.newTrackPoints ++ followingTrack
            in
            { track
                | trackPoints = TrackPoint.prepareTrackPoints recombinedTrack
            }

        Nothing ->
            track


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


highlightPoints : Color.Color -> List TrackPoint -> List (Entity LocalCoords)
highlightPoints color points =
    let
        material =
            Material.color color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 5 } material p.xyz
    in
    List.map highlightPoint points


type alias IntersectionInformation =
    { intersection : Point3d.Point3d Length.Meters LocalCoords
    , distanceAlong : Length.Length
    , tangentPoint : Point3d.Point3d Length.Meters LocalCoords
    , joinsBendAt : Point3d.Point3d Length.Meters LocalCoords
    }


preview : Model -> Track -> Model
preview model track =
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
            -- Look for points that could be pulled in to the bend, only when they
            -- are bracketed by interior points.
            let
                ( lowestInteriorIndex, highestInteriorIndex ) =
                    ( List.Extra.minimumBy .index pointsWithinCircle |> Maybe.map .index |> Maybe.withDefault 0
                    , List.Extra.maximumBy .index pointsWithinCircle |> Maybe.map .index |> Maybe.withDefault 0
                    )
            in
            SpatialIndex.queryWithFilter track.spatialIndex boundingBox isWithinDisc
                |> List.map .content
                |> List.filter (\p -> p.index >= startRange && p.index <= endRange)
                |> List.filter (\p -> p.index > lowestInteriorIndex && p.index < highestInteriorIndex)

        allPoints =
            List.sortBy .index (pointsWithinCircle ++ pointsWithinDisc)

        ( firstPoint, lastPoint ) =
            -- These bracket the points that will be removed, inclusively.
            ( List.head allPoints
            , List.Extra.last allPoints
            )

        -- Care over turn direction. Bend may exceed 180 degrees and Angle would not be good.
        -- Perhaps simpler and more reliable is "which side of the entry road is the centre?"
        isLeftHandBend =
            -- OK, not the entry road, but the first road segment in the circle.
            -- Positive distance == LEFT OF ROAD, i.e. left hand bend.
            -- Note I ignore the obvious "on the axis" case; this may bite me later.
            -- WHY do I end up with this fuggle? What's the better practice?
            case firstPoint of
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

        trackPointFromSegments segs =
            (List.map LineSegment3d.startPoint (List.take 1 segs)
                ++ List.map LineSegment3d.endPoint segs
            )
                |> List.map TrackPoint.trackPointFromPoint

        findEntryLineFromExistingPoint :
            Int
            -> Maybe IntersectionInformation
        findEntryLineFromExistingPoint index =
            -- By making this into a function we should be able to recurse "back down
            -- the track" until we find an acceptable tangent point or run out of track.
            let
                pointsDefiningEntryLine =
                    -- We know these points exist but, you know, type safety.
                    ( List.Extra.getAt (index - 1) track.trackPoints
                    , List.Extra.getAt index track.trackPoints
                    )

                -- Construct a parallel to the entry segment, 'r' meters towards the arc start.
                -- Where this intersects the '2r' circle, is centre of the entry bend.
                -- Where this new circle is tangent to the line segment is where the entry begins.
                -- If the entry precedes the segment start, we have failed to find a line;
                -- the user will need to move the marker. Or we recurse back down the track; that might work.
                entryLineSegment =
                    case pointsDefiningEntryLine of
                        ( Just prior, Just after ) ->
                            Just <|
                                LineSegment2d.from
                                    (Point3d.projectInto drawingPlane prior.xyz)
                                    (Point3d.projectInto drawingPlane after.xyz)

                        _ ->
                            Nothing

                entryLineAxis =
                    entryLineSegment
                        |> Maybe.andThen
                            (\l ->
                                Axis2d.throughPoints
                                    (LineSegment2d.startPoint l)
                                    (LineSegment2d.endPoint l)
                            )

                entryLineShiftVector =
                    let
                        shiftAmount =
                            if isLeftHandBend then
                                Quantity.negate model.pushRadius

                            else
                                model.pushRadius
                    in
                    case entryLineSegment of
                        Just seg ->
                            Maybe.map (Vector2d.withLength shiftAmount)
                                (LineSegment2d.perpendicularDirection seg)

                        Nothing ->
                            Nothing

                shiftedEntryLine =
                    case ( entryLineSegment, entryLineShiftVector ) of
                        ( Just theLine, Just theVector ) ->
                            Just <| LineSegment2d.translateBy theVector theLine

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
                                    { centre =
                                        centre
                                            |> Point3d.projectInto drawingPlane
                                            |> Point2d.toRecord Length.inMeters
                                    , radius = model.pushRadius |> Quantity.multiplyBy 2 |> Length.inMeters
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
                    case ( entryLineAxis, entryLineSegment ) of
                        ( Just sameOldAxis, Just sameOldSegment ) ->
                            let
                                elaborateIntersectionPoint i =
                                    let
                                        iOnPlane =
                                            Point3d.on drawingPlane i

                                        distanceAlong =
                                            Point2d.signedDistanceAlong sameOldAxis i

                                        tangentPoint2d =
                                            Point2d.along sameOldAxis distanceAlong

                                        tangentPoint3d =
                                            Point3d.on drawingPlane tangentPoint2d
                                    in
                                    { intersection = iOnPlane
                                    , distanceAlong = distanceAlong
                                    , tangentPoint = tangentPoint3d
                                    , joinsBendAt = Point3d.midpoint iOnPlane centre
                                    }
                            in
                            List.map elaborateIntersectionPoint outerCircleIntersections
                                |> List.Extra.minimumBy (.distanceAlong >> Length.inMeters)
                                |> Maybe.Extra.toList
                                |> List.filter (.distanceAlong >> Quantity.greaterThanOrEqualTo Quantity.zero)
                                |> List.filter (.distanceAlong >> Quantity.lessThanOrEqualTo (LineSegment2d.length sameOldSegment))

                        _ ->
                            []
            in
            case validCounterBendCentresAndTangentPoints of
                [] ->
                    if index > 1 then
                        findEntryLineFromExistingPoint (index - 1)

                    else
                        Nothing

                bestFound :: _ ->
                    Just bestFound

        --TODO: The real bend starts at the entry join point, not what we've previously thought.
        --TODO: Elevations, only holistic.
        --TODO: Same for exit bend, I hope with some common code!!
        entryCurve =
            case Maybe.andThen (.index >> findEntryLineFromExistingPoint) firstPoint of
                Just { intersection, distanceAlong, tangentPoint, joinsBendAt } ->
                    let
                        entryArcAxis =
                            Axis3d.withDirection Direction3d.positiveZ intersection

                        directionToEntryStart =
                            Direction3d.from intersection tangentPoint

                        directionToEntryEnd =
                            Direction3d.from intersection joinsBendAt

                        entryArcAngle =
                            Maybe.map2 Direction3d.angleFrom directionToEntryStart directionToEntryEnd
                                |> Maybe.withDefault (Angle.degrees 0)

                        entryArc =
                            Arc3d.sweptAround entryArcAxis entryArcAngle tangentPoint

                        entryArcLength =
                            Length.inMeters (Arc3d.radius entryArc)
                                * Angle.inRadians (Arc3d.sweptAngle entryArc)
                                |> abs

                        entryArcNumSegments =
                            entryArcLength
                                / Length.inMeters model.spacing
                                |> ceiling
                                |> max 1

                        entryArcSegments =
                            Arc3d.segments entryArcNumSegments entryArc |> Polyline3d.segments
                    in
                    entryArcSegments

                Nothing ->
                    []

        newBendEntirely =
            []
                ++ (entryCurve |> trackPointFromSegments)

        --++ (planarSegments |> trackPointFromSegments)
        --++ exitTransition
    in
    { model
        | pointsWithinCircle = pointsWithinCircle
        , pointsWithinDisc = pointsWithinDisc
        , circle = Just circle
        , pointsAreContiguous = areContiguous allPoints
        , newTrackPoints = newBendEntirely
        , fixedAttachmentPoints =
            case ( firstPoint, lastPoint ) of
                ( Just isFirst, Just isLast ) ->
                    Just ( isFirst.index, isLast.index )

                _ ->
                    Nothing
    }


getPreview : Model -> List (Entity LocalCoords)
getPreview model =
    showCircle model
        ++ showDisc model
        ++ highlightPoints Color.white model.pointsWithinCircle
        ++ highlightPoints Color.blue model.pointsWithinDisc
        ++ highlightPoints Color.lightYellow model.newTrackPoints


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
    (List.length points == 0)
        || (case ( List.maximum indices, List.minimum indices ) of
                ( Just isMax, Just isMin ) ->
                    (isMax - isMin == List.length points - 1) && List.Extra.allDifferent indices

                _ ->
                    False
           )
