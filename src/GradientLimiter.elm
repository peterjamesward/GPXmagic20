module GradientLimiter exposing (..)

import Angle
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PostUpdateActions
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
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

        ( targetLength, targetElevationChangeAA ) =
            ( endPoint.distanceFromStart |> Quantity.minus startPoint.distanceFromStart
            , endPoint.xyz |> Point3d.zCoordinate |> Quantity.minus (startPoint.xyz |> Point3d.zCoordinate)
            )

        undoMessage =
            "limit gradient\nfrom "
                ++ showDecimal0 (inMeters startPoint.distanceFromStart)
                ++ " to "
                ++ showDecimal0 (inMeters endPoint.distanceFromStart)
                ++ "."

        ( beforeEnd, afterEnd ) =
            List.Extra.splitAt (1 + endPoint.index) track.trackPoints

        ( beforeStart, targetZone ) =
            List.Extra.splitAt startPoint.index beforeEnd

        unclampedXYDeltas : List ( Length.Length, Length.Length )
        unclampedXYDeltas =
            List.map2
                (\pt1 pt2 ->
                    ( pt1.length
                    , Point3d.zCoordinate pt2.xyz |> Quantity.minus (Point3d.zCoordinate pt1.xyz)
                    )
                )
                targetZone
                (List.drop 1 targetZone)

        clampedXYDeltas : List ( Length.Length, Length.Length )
        clampedXYDeltas =
            List.map
                (\( x, y ) ->
                    ( x
                    , Quantity.clamp
                        (x |> Quantity.multiplyBy (negate settings.maximumDescent / 100.0))
                        (x |> Quantity.multiplyBy (settings.maximumAscent / 100.0))
                        y
                    )
                )
                unclampedXYDeltas

        targetElevationChange =
            Quantity.sum <| List.map Tuple.second unclampedXYDeltas

        clampedElevationChange =
            Quantity.sum <| List.map Tuple.second clampedXYDeltas

        elevationCorrection =
            targetElevationChange |> Quantity.minus clampedElevationChange

        offeredCorrections =
            -- "Ask" each segment how much leeway they have from the limit (up or down)
            if elevationCorrection |> Quantity.greaterThan Quantity.zero then
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumAscent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else if elevationCorrection |> Quantity.lessThan Quantity.zero then
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumDescent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else
                List.map (always Quantity.zero) clampedXYDeltas

        _ =
            Debug.log "unclamped" unclampedXYDeltas

        _ =
            Debug.log "clamped" clampedXYDeltas

        _ =
            Debug.log "offered" offeredCorrections

        totalOffered =
            Quantity.sum offeredCorrections

        proprtionNeeded =
            -- Assuming less than one for now, or button should have been disabled.
            if Quantity.abs elevationCorrection |> Quantity.lessThan (meters 0.1) then
                0

            else
                Quantity.ratio elevationCorrection totalOffered
                    |> clamp 0.0 1.0

        _ =
            Debug.log "needed, offered, ask" ( elevationCorrection, totalOffered, proprtionNeeded )

        proRataCorrections =
            -- Empirical test.
            List.map
                (Quantity.multiplyBy proprtionNeeded)
                offeredCorrections

        finalYDeltas =
            List.map2
                (\( x, y ) adjust -> y |> Quantity.plus adjust)
                clampedXYDeltas
                proRataCorrections

        resultingElevations =
            List.Extra.scanl
                Quantity.plus
                (Point3d.zCoordinate startPoint.xyz)
                finalYDeltas

        _ =
            Debug.log "starting els" <| List.map (.xyz >> Point3d.zCoordinate >> inMeters) targetZone

        _ =
            Debug.log "elevations" resultingElevations

        applyLimitsWithinRegion =
            List.map2
                (\pt ele ->
                    let
                        ( oldX, oldY, oldZ ) =
                            pt.xyz |> Point3d.coordinates

                        newXYZ =
                            Point3d.xyz oldX oldY ele
                    in
                    { pt | xyz = newXYZ }
                )
                targetZone
                resultingElevations
    in
    ( { track | trackPoints = beforeStart ++ applyLimitsWithinRegion ++ afterEnd }
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
                                ++ showDecimal0 options.maximumAscent
                                ++ "%"
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
                                ++ showDecimal0 options.maximumDescent
                                ++ "%"
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
