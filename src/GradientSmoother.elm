module GradientSmoother exposing (..)

import Angle
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Float.Extra exposing (interpolateFrom)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions exposing (EditResult, UndoEntry)
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


toolLabel =
    "Smooth gradient"


info =
    """## Gradient smoother

This is the 'classic' gradient-only smoother from v1.

At one extreme, it will provide a uniform gradient between
the two marker cones. At the other extreme, it will leave
all gradients unchanged. In-between, it will bring all gradients
closer to the uniform value, to some extent.

It will not affect spacing of points or bends. It can be 
useful just for taking the edge off of a rough stretch but
lacks any sophistication."""


type Msg
    = SmoothGradient Float
    | SetBumpinessFactor Float


type alias Options =
    { bumpinessFactor : Float
    }


type alias UndoRedoInfo =
    { regionStart : Int
    , regionEnd : Int
    , originalAltitudes : List Length.Length
    , revisedAltitudes : List Length.Length
    }


defaultOptions =
    { bumpinessFactor = 0.5 }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
update msg settings track =
    case msg of
        SetBumpinessFactor bumpiness ->
            ( { settings | bumpinessFactor = bumpiness }
            , PostUpdateActions.ActionNoOp
            )

        SmoothGradient bumpiness ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildActions settings track)
            )


averageGradient : TrackPoint -> TrackPoint -> Float
averageGradient startPoint endPoint =
    -- This is gradient measured along route, not as the crow flies.
    Direction3d.from startPoint.profileXZ endPoint.profileXZ
        |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
        |> Maybe.withDefault Quantity.zero
        |> Angle.tan
        |> (*) 100.0


averageSlopeLine : TrackPoint -> TrackPoint -> LineSegment3d Meters LocalCoords
averageSlopeLine pt1 pt2 =
    -- This is more useful when we wish to apply the gradient.
    LineSegment3d.from pt1.profileXZ pt2.profileXZ


buildActions : Options -> Track -> UndoEntry
buildActions options track =
    -- This feels like a simple foldl, creating a new list of TrackPoints
    -- which we then splice into the model.
    -- It's a fold because we must keep track of the current elevation
    -- which will increase with each segment.
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startPoint, endPoint ) =
            ( if track.currentNode.index <= marker.index then
                track.currentNode

              else
                marker
            , if track.currentNode.index > marker.index then
                track.currentNode

              else
                marker
            )

        undoMessage =
            "gradient smoothing\nfrom "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " to "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        slope =
            averageSlopeLine startPoint endPoint

        ( xAtStart, xLength ) =
            ( inMeters startPoint.distanceFromStart
            , inMeters endPoint.distanceFromStart
                - inMeters startPoint.distanceFromStart
            )

        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt startPoint.index

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (endPoint.index - startPoint.index)

        applyAverageWithinRegion =
            List.map applyAdjustment region

        applyAdjustment : TrackPoint -> Length.Length
        applyAdjustment pt =
            let
                smoothedElevation =
                    LineSegment3d.interpolate slope
                        ((inMeters pt.distanceFromStart - xAtStart) / xLength)
                        |> Point3d.zCoordinate
            in
            Quantity.interpolateFrom
                smoothedElevation
                (Point3d.zCoordinate pt.xyz)
                options.bumpinessFactor

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { regionStart = startPoint.index
            , regionEnd = endPoint.index
            , originalAltitudes = region |> List.map (.xyz >> Point3d.zCoordinate)
            , revisedAltitudes = applyAverageWithinRegion
            }
    in
    { label = "Smooth gradients"
    , editFunction = apply undoRedoInfo
    , undoFunction = undo undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


apply : UndoRedoInfo -> Track -> EditResult
apply undoRedoInfo track =
    let
        _ =
            Debug.log "info" undoRedoInfo

        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.regionStart

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (undoRedoInfo.regionEnd - undoRedoInfo.regionStart)

        adjusted =
            -- Make it so.
            List.map2
                (\pt ele ->
                    let
                        ( oldX, oldY, _ ) =
                            pt.xyz |> Point3d.coordinates

                        newXYZ =
                            Point3d.xyz oldX oldY ele
                    in
                    { pt | xyz = newXYZ }
                )
                region
                undoRedoInfo.revisedAltitudes
    in
    { before = prefix
    , edited = adjusted
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


undo : UndoRedoInfo -> Track -> EditResult
undo undoRedoInfo track =
    let
        ( prefix, theRest ) =
            track.trackPoints
                |> List.Extra.splitAt undoRedoInfo.regionStart

        ( region, suffix ) =
            theRest
                |> List.Extra.splitAt (undoRedoInfo.regionEnd - undoRedoInfo.regionStart)

        adjusted =
            -- Make it so.
            List.map2
                (\pt ele ->
                    let
                        ( oldX, oldY, _ ) =
                            pt.xyz |> Point3d.coordinates

                        newXYZ =
                            Point3d.xyz oldX oldY ele
                    in
                    { pt | xyz = newXYZ }
                )
                region
                undoRedoInfo.originalAltitudes
    in
    { before = prefix
    , edited = adjusted
    , after = suffix
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


viewGradientFixerPane : Options -> (Msg -> msg) -> Track -> Element msg
viewGradientFixerPane options wrapper track =
    let
        smoothnessSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetBumpinessFactor
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Bumpiness = "
                                ++ showDecimal2 options.bumpinessFactor
                , min = 0.0
                , max = 1.0
                , step = Nothing
                , value = options.bumpinessFactor
                , thumb = Input.defaultThumb
                }

        markedNode =
            Maybe.withDefault track.currentNode track.markedNode

        startPoint =
            if track.currentNode.index <= markedNode.index then
                track.currentNode

            else
                markedNode

        endPoint =
            if track.currentNode.index < markedNode.index then
                markedNode

            else
                track.currentNode

        avg =
            averageGradient startPoint endPoint
    in
    wrappedRow [ spacing 10, padding 10 ]
        [ button
            prettyButtonStyles
            { onPress = Just <| wrapper <| SmoothGradient options.bumpinessFactor
            , label =
                text <|
                    "Smooth between markers\nAverage gradient "
                        ++ showDecimal2 avg
            }
        , smoothnessSlider
        ]
