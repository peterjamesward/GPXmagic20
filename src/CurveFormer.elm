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
            --TODO: Exclude outliers at either end.
            SpatialIndex.queryWithFilter track.spatialIndex boundingBox isWithinDisc
                |> List.map .content
                |> List.filter (\p -> p.index >= startRange && p.index <= endRange)

        allPoints =
            List.sortBy .index (pointsWithinCircle ++ pointsWithinDisc)

        ( from, to ) =
            let
                allIndices =
                    List.map .index allPoints
            in
            ( List.minimum allIndices |> Maybe.withDefault 0
            , List.maximum allIndices |> Maybe.withDefault 0
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

        planarPoints =
            -- We must work in 2d to achieve the smooth arc for the whole bend.
            -- Use circle centre, first and last new points.
            -- May be convenient to use a 3d arc nontheless.
            previewTrackPoints
                |> List.map
                    (\p -> p.xyz |> Point3d.projectInto drawingPlane)

        planarCentre =
            centre |> Point3d.projectInto drawingPlane

        planarArc =
            -- For piecewise, use list of arcs. This becomes degenerate case.
            case
                ( List.head planarPoints
                , List.Extra.getAt 1 planarPoints
                , List.Extra.last planarPoints
                )
            of
                ( Just firstPoint, Just secondPoint, Just lastPoint ) ->
                    Arc2d.throughPoints firstPoint secondPoint lastPoint

                _ ->
                    Nothing

        planarSegments =
            case planarArc of
                Just arc ->
                    let
                        arc3d =
                            Arc3d.on drawingPlane arc

                        arcLength =
                            Length.inMeters (Arc2d.radius arc)
                                * Angle.inRadians (Arc2d.sweptAngle arc)
                                |> abs

                        numSegments =
                            arcLength
                                / Length.inMeters model.spacing
                                |> ceiling
                                |> max 1

                        segments =
                            Arc3d.segments numSegments arc3d |> Polyline3d.segments
                    in
                    segments

                Nothing ->
                    []

        trackPointFromSegments segs =
            (List.map LineSegment3d.startPoint (List.take 1 segs)
                ++ List.map LineSegment3d.endPoint segs
            )
                |> List.map TrackPoint.trackPointFromPoint

        -- TODO: New logic here for entry & exit lines based on line/circle intersections as per notes.
        -- elm-geometry doesn't help; algebra is better for this IMHO.
        pointsDefiningEntryLine =
            -- We know these points exist but, you know, type safety.
            ( List.Extra.getAt (from - 1) track.trackPoints
            , List.Extra.getAt from track.trackPoints
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
                    (\l -> Axis2d.throughPoints (LineSegment2d.startPoint l) (LineSegment2d.endPoint l))

        whichWayToOffset =
            -- 50-50 chance of needing the test flipped.
            Maybe.map2
                (\arc ax ->
                    Arc2d.startPoint arc
                        |> Point2d.signedDistanceFrom ax
                        |> Quantity.greaterThanOrEqualTo Quantity.zero
                )
                planarArc
                entryLineAxis
                |> Maybe.withDefault True

        entryLineShiftVector =
            case entryLineSegment of
                Just seg ->
                    Maybe.map (Vector2d.withLength model.pushRadius)
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

        _ = Debug.log "outerCircleIntersections" outerCircleIntersections

        validTangentPoints =
            -- Point is 'valid' if it is not 'before' the segment start (or axis origin).
            List.map (Point3d.on drawingPlane) <|
                case entryLineAxis of
                    Just sameOldAxis ->
                        outerCircleIntersections
                            |> List.filter
                                (\pt ->
                                    Point2d.signedDistanceAlong sameOldAxis pt
                                        |> Quantity.greaterThanOrEqualTo Quantity.zero
                                )

                    Nothing ->
                        []

        _ =
            Debug.log "Tangent points" validTangentPoints

        newBendEntirely =
            []
                --++ entryTransition
                ++ List.map TrackPoint.trackPointFromPoint validTangentPoints
                ++ (planarSegments |> trackPointFromSegments)

        --++ exitTransition
    in
    { model
        | pointsWithinCircle = pointsWithinCircle
        , pointsWithinDisc = pointsWithinDisc
        , circle = Just circle
        , pointsAreContiguous = areContiguous allPoints
        , newTrackPoints = newBendEntirely
        , fixedAttachmentPoints = Just ( from, to )
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
    (List.length points == 0)
        || (case ( List.maximum indices, List.minimum indices ) of
                ( Just isMax, Just isMin ) ->
                    (isMax - isMin == List.length points - 1) && List.Extra.allDifferent indices

                _ ->
                    False
           )
