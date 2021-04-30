module GradientSmoother exposing (..)

import Angle
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d exposing (LineSegment3d)
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


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


defaultOptions =
    { bumpinessFactor = 0.5 }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction )
update msg settings track =
    case msg of
        SetBumpinessFactor bumpiness ->
            ( { settings | bumpinessFactor = bumpiness }
            , PostUpdateActions.ActionNoOp
            )

        SmoothGradient bumpiness ->
            let
                ( newTrack, undoMsg ) =
                    smoothGradient track bumpiness
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                newTrack
                undoMsg
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


smoothGradient : Track -> Float -> ( Track, String )
smoothGradient track bumpiness =
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

        applyAverageWithinRegion =
            List.map applyAdjustment track.track

        applyAdjustment : TrackPoint -> TrackPoint
        applyAdjustment pt =
            -- This reads nicer than the v1 splicing and folding method.
            if pt.index > startPoint.index && pt.index < endPoint.index then
                let
                    current =
                        Point3d.toRecord inMeters pt.xyz

                    smoothedElevation =
                        LineSegment3d.interpolate slope
                            ((inMeters pt.distanceFromStart - xAtStart) / xLength)
                            |> Point3d.zCoordinate
                            |> inMeters

                    smoothedPoint =
                        Point3d.fromRecord meters { current | z = smoothedElevation }

                    blendedElevation =
                        Point3d.interpolateFrom smoothedPoint pt.xyz bumpiness
                in
                { pt | xyz = blendedElevation }

            else
                pt
    in
    ( { track | track = applyAverageWithinRegion }
    , undoMessage
    )


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

        gradientSmoothControls =
            row [ spacing 5, padding 5 ]
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
    in
    column [ padding 10, spacing 10, centerX ] <|
        [ gradientSmoothControls ]
