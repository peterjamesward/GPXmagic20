module Filters exposing (..)

import BezierSplines exposing (bezierSplines)
import Element exposing (..)
import Element.Input as Input exposing (button)
import List.Extra
import LoopedTrack exposing (Loopiness(..))
import Maybe.Extra
import Point3d exposing (Point3d)
import PostUpdateActions exposing (EditResult, UndoEntry)
import Quantity
import TabCommonElements exposing (wholeTrackTextHelper)
import Track exposing (Track)
import TrackObservations exposing (TrackObservations)
import TrackPoint exposing (TrackPoint, temporaryIndices, trackPointFromPoint)
import Triangle3d exposing (Triangle3d)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, prettyButtonStyles)


toolLabel =
    "Track smoothers 3D"


info =
    """## Filters

Centroid averaging reduces the amount of wiggle in a track,
vertically and horizontally, by moving each track point towards
the centre of the triangle formed by the neighbouring points.

You may achieve better results if you first increase the density
of trackpoints in the region by using Track Point Insert.

Bezier splines can work quite well where there are relatively
few track points -- it tries to find a flowing line that passses
through each existing point. The two parametes affect the number
of points it will add to smooth the curves and the 'tension' of
the thread it uses. It's best to play, but the defaults are OK.

Curiously, using a spline and then averaging can produce quite
pleasing results."""


type Msg
    = SetFilterBias Float
    | FilterWeightedAverage
    | SetBezierTension Float
    | SetBezierTolerance Float
    | BezierSplines
    | SetPositionFlag Bool
    | SetElevationFlag Bool
    | BezierApproximation


type alias Options =
    { filterBias : Float
    , bezierTension : Float
    , bezierTolerance : Float
    , applyToPosition : Bool
    , applyToElevation : Bool
    }


defaultOptions : Options
defaultOptions =
    { filterBias = 50.0
    , bezierTension = 0.5
    , bezierTolerance = 5.0
    , applyToPosition = True
    , applyToElevation = True
    }


type alias UndoRedoInfo =
    { start : Int
    , fromEnd : Int
    , originalPoints : List TrackPoint
    , isLoop : Bool
    , splineFunction : List TrackPoint -> List TrackPoint
    }


update :
    Msg
    -> Options
    -> TrackObservations
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
update msg settings observations track =
    case msg of
        SetFilterBias bias ->
            ( { settings | filterBias = bias }
            , PostUpdateActions.ActionNoOp
            )

        SetBezierTension tension ->
            ( { settings | bezierTension = tension }
            , PostUpdateActions.ActionNoOp
            )

        SetBezierTolerance tolerance ->
            ( { settings | bezierTolerance = tolerance }
            , PostUpdateActions.ActionNoOp
            )

        SetPositionFlag position ->
            ( { settings | applyToPosition = position }
            , PostUpdateActions.ActionNoOp
            )

        SetElevationFlag elevation ->
            ( { settings | applyToElevation = elevation }
            , PostUpdateActions.ActionNoOp
            )

        FilterWeightedAverage ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildCentroidAverageActions observations settings track)
            )

        BezierSplines ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildBezierActions BezierSplines.bezierSplines observations settings track)
            )

        BezierApproximation ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildBezierActions BezierSplines.bezierApproximation observations settings track)
            )


viewFilterControls : Options -> (Msg -> msg) -> Track -> Element msg
viewFilterControls options wrap track =
    let
        centroidFilterControls =
            [ Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetFilterBias
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Smoothing "
                                ++ String.fromInt (round options.filterBias)
                                ++ "%"
                , min = 0.0
                , max = 100.0
                , step = Just 1.0
                , value = options.filterBias
                , thumb = Input.defaultThumb
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap FilterWeightedAverage
                , label = text <| "Centroid averaging"
                }
            , Input.checkbox [ moveRight 40 ]
                { onChange = wrap << SetPositionFlag
                , icon = checkboxIcon
                , checked = options.applyToPosition
                , label = Input.labelRight [ centerY ] (text "Position")
                }
            , Input.checkbox [ moveRight 40 ]
                { onChange = wrap << SetElevationFlag
                , icon = checkboxIcon
                , checked = options.applyToElevation
                , label = Input.labelRight [ centerY ] (text "Elevation")
                }
            ]

        bezierControls =
            [ Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetBezierTension
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Tension "
                                ++ showDecimal2 options.bezierTension
                , min = 0.0
                , max = 1.0
                , step = Just 0.1
                , value = options.bezierTension
                , thumb = Input.defaultThumb
                }
            , Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetBezierTolerance
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Tolerance "
                                ++ showDecimal2 options.bezierTolerance
                , min = 1.0
                , max = 10.0
                , step = Just 0.5
                , value = options.bezierTolerance
                , thumb = Input.defaultThumb
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap BezierSplines
                , label = text <| "Bezier splines passing\nthrough existing points"
                }
            , button
                prettyButtonStyles
                { onPress = Just <| wrap BezierApproximation
                , label = text <| "Bezier approximation\nusing existing points"
                }
            ]
    in
    column [ spacing 10, padding 10, centerX, width fill ]
        [ text "Centroid averaging reduces local deviations."
        , wrappedRow [ spacing 10, padding 10 ] centroidFilterControls
        , text "Splines create new points to smooth between existing points."
        , wrappedRow [ spacing 10, padding 10 ] bezierControls
        , wholeTrackTextHelper track
        ]


type alias FilterFunction =
    (TrackPoint -> Float)
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> Float


smoothWithDefaults : Track -> List TrackPoint
smoothWithDefaults track =
    -- Helper for One-Click-Quick-Fix
    let
        actions =
            buildCentroidAverageActions { loopiness = NotALoop Quantity.zero } defaultOptions track

        results =
            actions.editFunction track
    in
    results.edited |> TrackPoint.prepareTrackPoints


buildCentroidAverageActions : { a | loopiness : Loopiness } -> Options -> Track -> UndoEntry
buildCentroidAverageActions observations options track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        trackLength =
            List.length track.trackPoints

        ( startPoint, endPoint ) =
            if track.markedNode == Nothing then
                ( 0, trackLength - 1 )

            else if track.currentNode.index <= marker.index then
                ( track.currentNode.index, marker.index )

            else
                ( marker.index, track.currentNode.index )

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { start = startPoint
            , fromEnd = trackLength - endPoint
            , originalPoints =
                track.trackPoints
                    |> List.take (endPoint + 1)
                    |> List.drop startPoint
            , isLoop = observations.loopiness == IsALoop
            , splineFunction = identity
            }
    in
    { label = "Centroid average"
    , editFunction = applyCentroidAverage options undoRedoInfo
    , undoFunction = undoCentroidAverage options undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


applyCentroidAverage : Options -> UndoRedoInfo -> Track -> EditResult
applyCentroidAverage options undoRedo track =
    let
        points =
            track.trackPoints

        trackLength =
            List.length points

        ( prefix, theRest ) =
            -- These possible one outs.
            points |> List.Extra.splitAt undoRedo.start

        ( withinRange, suffix ) =
            -- These possible one outs.
            theRest |> List.Extra.splitAt (trackLength - undoRedo.fromEnd)

        ( firstPoint, lastPoint ) =
            -- These are points outside the range so are not affected but are needed
            -- for the filters on each end of the region.
            if undoRedo.isLoop && track.markedNode == Nothing then
                -- This is used for wrap-around on loop, in which case use second point
                ( withinRange |> List.Extra.getAt 2 |> Maybe.Extra.toList
                , withinRange |> List.Extra.getAt (List.length withinRange - 2) |> Maybe.Extra.toList
                )

            else
                ( prefix |> List.Extra.getAt (List.length prefix - 1) |> Maybe.Extra.toList
                , suffix |> List.take 1
                )

        filteredPoints =
            if track.markedNode == Nothing && undoRedo.isLoop then
                -- Whole track, wraps around for a loop.
                List.map3
                    (weightedAverage options)
                    (lastPoint ++ points)
                    points
                    (List.drop 1 points ++ firstPoint)

            else
                List.map3
                    (weightedAverage options)
                    (firstPoint ++ withinRange)
                    withinRange
                    (List.drop 1 withinRange ++ lastPoint)
    in
    { before = prefix
    , edited = filteredPoints
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


undoCentroidAverage : Options -> UndoRedoInfo -> Track -> EditResult
undoCentroidAverage options undoRedo track =
    let
        points =
            track.trackPoints

        trackLength =
            List.length points

        ( prefix, theRest ) =
            -- These possible one outs.
            points |> List.Extra.splitAt undoRedo.start

        ( withinRange, suffix ) =
            -- These possible one outs.
            theRest |> List.Extra.splitAt (trackLength - undoRedo.fromEnd)
    in
    { before = prefix
    , edited = undoRedo.originalPoints
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


bezierWithDefaults : Track -> List TrackPoint
bezierWithDefaults track =
    -- Helper for One-Click-Quick-Fix
    let
        actions =
            buildBezierActions
                BezierSplines.bezierApproximation
                { loopiness = NotALoop Quantity.zero }
                defaultOptions
                track

        results =
            actions.editFunction track
    in
    results.edited |> TrackPoint.prepareTrackPoints


buildBezierActions :
    (Bool -> Float -> Float -> List TrackPoint -> List TrackPoint)
    -> { a | loopiness : Loopiness }
    -> Options
    -> Track
    -> UndoEntry
buildBezierActions splineFunction observations options track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        trackLength =
            List.length track.trackPoints

        ( startPoint, endPoint ) =
            if track.markedNode == Nothing then
                ( 0, trackLength - 1 )

            else if track.currentNode.index <= marker.index then
                ( track.currentNode.index, marker.index )

            else
                ( marker.index, track.currentNode.index )

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { start = startPoint
            , fromEnd = trackLength - endPoint
            , originalPoints =
                track.trackPoints
                    |> List.take endPoint
                    |> List.drop (startPoint - 1)
            , isLoop = observations.loopiness == IsALoop
            , splineFunction =
                splineFunction
                    (observations.loopiness == IsALoop)
                    options.bezierTension
                    options.bezierTolerance
            }
    in
    { label = "Bezier splines"
    , editFunction = applyBezierSpline undoRedoInfo
    , undoFunction = undoBezierSpline undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


applyBezierSpline : UndoRedoInfo -> Track -> EditResult
applyBezierSpline undoRedo track =
    --TODO: May need to puyt back the special code for loops.
    let
        points =
            track.trackPoints

        trackLength = List.length points

        ( theRest, suffix ) =
            points |> List.Extra.splitAt (trackLength - undoRedo.fromEnd)

        ( prefix, region ) =
            theRest |> List.Extra.splitAt (undoRedo.start - 1)

        splinedSection =
            case ( undoRedo.isLoop, track.markedNode ) of
                ( True, Nothing ) ->
                    undoRedo.splineFunction points

                ( _, _ ) ->
                    undoRedo.splineFunction region
    in
    { before = prefix
    , edited = splinedSection
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


undoBezierSpline : UndoRedoInfo -> Track -> EditResult
undoBezierSpline undoRedo track =
    let
        points =
            track.trackPoints

        trackLength = List.length points

        ( theRest, suffix ) =
            points |> List.Extra.splitAt (trackLength - undoRedo.fromEnd)

        ( prefix, region ) =
            theRest |> List.Extra.splitAt (undoRedo.start - 1)
    in
    { before = prefix
    , edited = undoRedo.originalPoints
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


weightedAverage :
    Options
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
    -> TrackPoint
weightedAverage settings p0 p1 p2 =
    let
        mergePositionWithElevation pXY pZ =
            let
                sourceXY =
                    Point3d.toMeters pXY

                sourceZ =
                    Point3d.toMeters pZ
            in
            { sourceXY | z = sourceZ.z } |> Point3d.fromMeters

        triangle =
            Triangle3d.fromVertices ( p0.xyz, p1.xyz, p2.xyz )

        centroid =
            Triangle3d.centroid triangle

        newP1 =
            Point3d.interpolateFrom p1.xyz centroid (settings.filterBias / 100.0)

        withFlags =
            case ( settings.applyToPosition, settings.applyToElevation ) of
                ( True, True ) ->
                    newP1

                ( False, False ) ->
                    p1.xyz

                ( True, False ) ->
                    -- Use new position, old elevation
                    mergePositionWithElevation newP1 p1.xyz

                ( False, True ) ->
                    mergePositionWithElevation p1.xyz newP1
    in
    trackPointFromPoint withFlags



-- END
