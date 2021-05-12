module GradientLimiter exposing (..)

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
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Gradient limiter

## New in 2.1.5

Mark two points on the route. The elevation at these points will be fixed.
Set the maximum ascent and descent with the sliders.
Click the button to redistribute the ascents and descents between the 
markers.
If it's not arithmetically possible, the button will not work.
"""


type Msg
    = LimitGradient
    | SetMaximumAscent Float
    | SetMaximumDescent Float


type alias Options =
    { maximumAscent : Float
    , maximumDescent : Float
    }


defaultOptions : Options
defaultOptions =
    { maximumAscent = 20.0
    , maximumDescent = 20.0
    }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction msg )
update msg settings track =
    case msg of
        SetMaximumAscent up ->
            ( { settings | maximumAscent = up }
            , PostUpdateActions.ActionNoOp
            )

        SetMaximumDescent down ->
            ( { settings | maximumDescent = down }
            , PostUpdateActions.ActionNoOp
            )

        LimitGradient ->
            let
                ( newTrack, undoMsg ) =
                    limitGradient settings track
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


limitGradient : Options -> Track -> ( Track, String )
limitGradient settings track =
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
            "limit gradient\nfrom "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " to "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        ( xAtStart, xLength ) =
            ( inMeters startPoint.distanceFromStart
            , inMeters endPoint.distanceFromStart
                - inMeters startPoint.distanceFromStart
            )

        applyLimitsWithinRegion =
            track.trackPoints
    in
    ( { track | trackPoints = applyLimitsWithinRegion }
    , undoMessage
    )


viewGradientLimitPane : Options -> (Msg -> msg) -> Track -> Element msg
viewGradientLimitPane options wrapper track =
    let
        maxAscentSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaximumAscent
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Uphill: "
                                ++ showDecimal2 options.maximumAscent
                , min = 10.0
                , max = 25.0
                , step = Just 1.0
                , value = options.maximumAscent
                , thumb = Input.defaultThumb
                }

        maxDescentSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaximumDescent
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Downhill: "
                                ++ showDecimal2 options.maximumDescent
                , min = 10.0
                , max = 25.0
                , step = Just 1.0
                , value = options.maximumDescent
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
    in
    column [ spacing 5, padding 5, centerX ]
        [ row [ spacing 5, padding 5 ]
            [ maxAscentSlider
            , maxDescentSlider
            ]
        , if startPoint.index == endPoint.index then
            text "Please position the markers"

          else
            button
                prettyButtonStyles
                { onPress = Just <| wrapper <| LimitGradient
                , label =
                    text <|
                        "Apply limits"
                }
        ]
