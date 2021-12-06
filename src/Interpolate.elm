module Interpolate exposing (..)

import Color
import DisplayOptions exposing (DisplayOptions)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (TrackEditType(..), UndoEntry)
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints)
import SceneBuilderProfile exposing (highlightPointsProfile)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, trackPointFromPoint)
import Utils exposing (showDecimal0, showShortMeasure)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


toolLabel =
    "Interpolate"


info =
    """## Insert

Often, with planned routes, track points are too widely spaced
to give the control you need near problem areas.

With this tool, you can insert track points so that they have a
guaranteed maximum spacing. You can then use other tools to form
the shapes you seek.

This is good to use before the Bend Smoother or the Centroid filter, 
for example."""


type Msg
    = SetMaxSpacing Float
    | InsertPoints


type alias Options =
    { maxSpacing : Float }


defaultOptions =
    { maxSpacing = 10.0 }


type alias UndoRedoInfo =
    -- We should be able to Undo by working out the new points afresh.
    -- The options will also be available in the closure.
    { start : Int
    , end : Int
    , originalPoints : List TrackPoint
    , spacing : Float
    , newEnd : Int
    }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        SetMaxSpacing spacing ->
            ( { settings | maxSpacing = spacing }
            , PostUpdateActions.ActionPreview
            )

        InsertPoints ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                EditPreservesNodePosition
                (buildActions settings track)
            )


viewTools : Bool -> Options -> (Msg -> msg) -> Element msg
viewTools imperial options wrap =
    let
        splitSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrap << SetMaxSpacing
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Maximum gap "
                                ++ showShortMeasure imperial (Length.meters options.maxSpacing)
                , min =
                    Length.inMeters <|
                        if imperial then
                            Length.feet 3.0

                        else
                            Length.meters 1.0
                , max =
                    Length.inMeters <|
                        if imperial then
                            Length.feet 160.0

                        else
                            Length.meters 50.0
                , step = Nothing
                , value = options.maxSpacing
                , thumb = Input.defaultThumb
                }
    in
    row [ spacing 10, padding 10 ]
        [ splitSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrap InsertPoints
            , label = text "Insert points"
            }
        ]


buildActions : Options -> Track -> UndoEntry
buildActions options track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.currentNode.index <= marker.index then
                ( track.currentNode, marker )

            else
                ( marker, track.currentNode )

        undoMessage =
            "Interpolate from "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " to "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        originalPoints =
            track.trackPoints |> List.take (1 + endPoint.index) |> List.drop startPoint.index

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { start = startPoint.index
            , end = endPoint.index
            , originalPoints = originalPoints
            , spacing = options.maxSpacing
            , newEnd = 0
            }

        ( _, newPoints, _ ) =
            -- Yes, dummy run just to get the length.
            apply undoRedoInfo track

        revisedUndoRedo =
            { undoRedoInfo
                | newEnd =
                    endPoint.index - List.length originalPoints + List.length newPoints
            }

        newOrange =
            if track.currentNode.index == endPoint.index then
                endPoint.index + List.length newPoints - List.length originalPoints

            else
                track.currentNode.index

        newPurple =
            if track.markedNode == Just endPoint then
                Just (endPoint.index + List.length newPoints - List.length originalPoints)

            else
                Maybe.map .index track.markedNode
    in
    { label = undoMessage
    , editFunction = apply revisedUndoRedo
    , undoFunction = undo revisedUndoRedo
    , newOrange = newOrange
    , newPurple = newPurple
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


apply : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
apply undoRedo track =
    -- Introduce additional trackpoints in all segments **between** markers.
    -- Yes, there's some redundant track splitting. Not worth worrying about.
    let
        newPointsBetween pt1 pt2 =
            let
                trackPointsNeeded =
                    -- Including the final one.
                    -- Replacing this should make it easier for the multiple segment case.
                    ceiling <|
                        (inMeters <| Vector3d.length pt1.roadVector)
                            / undoRedo.spacing
            in
            List.map
                (\i ->
                    Point3d.interpolateFrom
                        pt1.xyz
                        pt2.xyz
                        (toFloat i / toFloat trackPointsNeeded)
                )
                (List.range 1 trackPointsNeeded)

        pointsToInterpolate =
            track.trackPoints
                |> List.take (undoRedo.end + 1)
                |> List.drop undoRedo.start

        allNewTrackPoints =
            List.map2
                newPointsBetween
                pointsToInterpolate
                (List.drop 1 pointsToInterpolate)
                |> List.concat
                |> List.map trackPointFromPoint

        precedingTrackPoints =
            List.take undoRedo.start track.trackPoints

        firstTrackPointOfInterpolatedSection =
            List.take 1 pointsToInterpolate

        subsequentTrackPoints =
            List.drop (undoRedo.end + 1) track.trackPoints
    in
    ( precedingTrackPoints
    , firstTrackPointOfInterpolatedSection ++ allNewTrackPoints
    , subsequentTrackPoints
    )


undo : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
undo undoRedo track =
    -- In Undo, we work out which are the new ones, so we can remove them.
    let
        pointsToDeinterpolate =
            track.trackPoints
                |> List.take undoRedo.newEnd
                |> List.drop undoRedo.start

        precedingTrackPoints =
            List.take undoRedo.start track.trackPoints

        subsequentTrackPoints =
            List.drop undoRedo.newEnd track.trackPoints
    in
    ( precedingTrackPoints, undoRedo.originalPoints, subsequentTrackPoints )


pointsIn referenceList testList =
    testList
        |> List.filter
            (\pt1 ->
                List.Extra.find
                    (\pt2 ->
                        Point3d.equalWithin Length.centimeter pt1.xyz pt2.xyz
                    )
                    referenceList
                    /= Nothing
            )


pointsNotIn referenceList testList =
    testList
        |> List.filter
            (\pt1 ->
                List.Extra.find
                    (\pt2 ->
                        Point3d.equalWithin Length.centimeter pt1.xyz pt2.xyz
                    )
                    referenceList
                    == Nothing
            )


getPreview3D : Options -> Track -> List (Entity LocalCoords)
getPreview3D options track =
    -- To heck with the expense here.
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            if track.currentNode.index <= marker.index then
                ( track.currentNode, marker )

            else
                ( marker, track.currentNode )

        originalPoints =
            track.trackPoints |> List.take (1 + endPoint.index) |> List.drop startPoint.index

        actions =
            buildActions options track

        ( _, points, _ ) =
            actions.editFunction track
    in
    highlightPoints Color.white (points |> pointsNotIn originalPoints)


interpolateWithDefaults : Track -> List TrackPoint
interpolateWithDefaults track =
    -- Helper for One-Click-Quick-Fix
    let
        actions =
            buildActions defaultOptions track

        ( _, points, _ ) =
            actions.editFunction track
    in
    points |> TrackPoint.prepareTrackPoints