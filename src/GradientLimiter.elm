module GradientLimiter exposing (..)

import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters, meters)
import List.Extra
import Point3d
import PostUpdateActions exposing (UndoEntry)
import Quantity
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


toolLabel =
    "Limit gradients"


info =
    """## Gradient limiter

Limit the uphill and downhill gradient across all or part of the route.
To select part of the route, use the Orange and Purple pointers.

If you do not place the Puple pointer, the end of the route will be used instead,
so effectively limiting from the Orange pointer to the route end.
The elevation at these points will be fixed.

Set the maximum ascent and descent with the sliders.

Click the button to redistribute the ascents and descents between the
markers.

If it's not arithmetically possible, the button will still work, but it may not come out well.
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
    { maximumAscent = 15.0
    , maximumDescent = 15.0
    }


type alias UndoRedoInfo =
    { regionStart : Int
    , regionEnd : Int
    , originalAltitudes : List Length.Length
    , revisedAltitudes : List Length.Length
    }


update :
    Msg
    -> Options
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck msg )
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
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildActions settings track)
            )


buildActions : Options -> Track -> UndoEntry
buildActions settings track =
    let
        marker =
            Maybe.withDefault track.currentNode track.markedNode

        ( startIndex, endIndex, referenceNode ) =
            if track.markedNode == Nothing then
                ( track.currentNode.index, List.length track.trackPoints - 1, track.currentNode )

            else if track.currentNode.index <= marker.index then
                ( track.currentNode.index, marker.index, track.currentNode )

            else
                ( marker.index, track.currentNode.index, marker )

        ( beforeEnd, afterEnd ) =
            -- Note we must include point after the end, I think, so we can do a map2.
            List.Extra.splitAt (1 + endIndex) track.trackPoints

        ( beforeStart, targetZone ) =
            List.Extra.splitAt startIndex beforeEnd

        unclampedXYDeltas : List ( Length.Length, Length.Length )
        unclampedXYDeltas =
            -- Yields X and Y deltas looking forward from each track point.
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
            -- What the deltas would be with the ascent and descent limits applied.
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
            -- Current change of elevation, derived directly by summation.
            Quantity.sum <| List.map Tuple.second unclampedXYDeltas

        clampedElevationChange =
            -- What the change would be with the limits in place.
            Quantity.sum <| List.map Tuple.second clampedXYDeltas

        elevationCorrection =
            -- What overall impact do the limits have?
            targetElevationChange |> Quantity.minus clampedElevationChange

        offeredCorrections =
            -- "Ask" each segment how much leeway they have from the limit (up or down)
            if elevationCorrection |> Quantity.greaterThan Quantity.zero then
                -- We need to gain height overall.
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumAscent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else if elevationCorrection |> Quantity.lessThan Quantity.zero then
                -- We need to lose height overall.
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumDescent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else
                List.map (always Quantity.zero) clampedXYDeltas

        totalOffered =
            -- How much do we have to play with?
            Quantity.sum offeredCorrections

        proprtionNeeded =
            -- Assuming less than one for now, or button should have been disabled.
            if Quantity.abs elevationCorrection |> Quantity.lessThan (meters 0.1) then
                -- 10 cm is near enough.
                0

            else
                -- How much of what is available is needed?
                Quantity.ratio elevationCorrection totalOffered
                    |> clamp 0.0 1.0

        proRataCorrections =
            -- What shall we ask from each segment, on this basis?
            List.map
                (Quantity.multiplyBy proprtionNeeded)
                offeredCorrections

        finalYDeltas =
            -- What does that make the deltas?
            List.map2
                (\( x, y ) adjust -> y |> Quantity.plus adjust)
                clampedXYDeltas
                proRataCorrections

        resultingElevations =
            -- And from that, the running cumulative elevations?
            List.Extra.scanl
                Quantity.plus
                (Point3d.zCoordinate referenceNode.xyz)
                finalYDeltas

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { regionStart = startIndex
            , regionEnd = endIndex
            , originalAltitudes = targetZone |> List.map (.xyz >> Point3d.zCoordinate)
            , revisedAltitudes = resultingElevations
            }
    in
    { label = "Limit gradients"
    , editFunction = apply undoRedoInfo
    , undoFunction = undo undoRedoInfo
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


apply : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
    ( prefix, adjusted, suffix )


undo : UndoRedoInfo -> Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )
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
    ( prefix, adjusted, suffix )


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
    wrappedRow [ spacing 10, padding 10 ]
        [ maxAscentSlider
        , maxDescentSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| LimitGradient
            , label =
                text <|
                    "Apply limits"
            }
        ]
