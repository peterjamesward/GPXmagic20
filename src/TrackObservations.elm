module TrackObservations exposing (..)

import Angle exposing (Angle)
import BendSmoother
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (inMeters, meters)
import List.Extra
import LoopedTrack exposing (Loopiness(..))
import Point3d
import PostUpdateActions
import Quantity exposing (zero)
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint, gradientFromPoint)
import Utils exposing (showDecimal0, showDecimal2, showLabelledValues)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Bend and Gradient problems

Similar in concept, these tabs help you find the worst
issues on the route. They will list places where the gradient
or direction changes by more than the threshold you choose.

Click on an entry to centre the views on that point.

Use __Smooth these points...__ to apply a simple fix to all the points listed
that are within the range of the Orange and Purple markers. **Note:** this
is not recommended other than as a way to clean up minor issues."""


type Msg
    = LocateProblem TrackPoint
    | Autofix (List TrackPoint)
    | SetBearingChangeThreshold Angle
    | SetGradientChangeThreshold Float
    | SetGradientThreshold Float


type alias Options =
    { gradientChangeThreshold : Float -- In 'slope' units, 100 * tan angle.
    , directionChangeThreshold : Angle -- In range -pi to +pi.
    , gradientThreshold : Float
    }


defaultOptions =
    { gradientChangeThreshold = 10.0
    , directionChangeThreshold = Angle.degrees 90.0
    , gradientThreshold = 15.0
    }


type alias TrackObservations =
    { abruptBearingChanges : List TrackPoint
    , abruptGradientChanges : List TrackPoint
    , zeroLengths : List TrackPoint
    , loopiness : LoopedTrack.Loopiness
    , highestMetres : Float
    , lowestMetres : Float
    , trackLength : Float
    , climbingDistance : Float
    , descendingDistance : Float
    , totalClimbing : Float
    , totalDescending : Float
    , meanSpacing : Float
    }


defaultObservations : TrackObservations
defaultObservations =
    { abruptBearingChanges = []
    , abruptGradientChanges = []
    , zeroLengths = []
    , loopiness = NotALoop <| meters 0.0
    , highestMetres = 0.0
    , lowestMetres = 0.0
    , trackLength = 0.0
    , climbingDistance = 0.0
    , descendingDistance = 0.0
    , totalClimbing = 0.0
    , totalDescending = 0.0
    , meanSpacing = 0.0
    }


update :
    Msg
    -> Options
    -> Int
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction msg )
update msg settings numSegments track =
    case msg of
        LocateProblem trackPoint ->
            ( settings
            , PostUpdateActions.ActionFocusMove trackPoint
            )

        SetBearingChangeThreshold angle ->
            ( { settings | directionChangeThreshold = angle }
            , PostUpdateActions.ActionRerender
            )

        SetGradientChangeThreshold threshold ->
            ( { settings | gradientChangeThreshold = threshold }
            , PostUpdateActions.ActionRerender
            )

        SetGradientThreshold threshold ->
            ( { settings | gradientThreshold = threshold }
            , PostUpdateActions.ActionNoOp
            )

        Autofix trackPoints ->
            -- Apply the new 3D single point smoother.
            -- Must do this from furthest point first, as points are added!
            let
                indicesToSmooth =
                    trackPoints |> List.map .index |> List.reverse

                applyToSinglePointByIndex : Int -> Track -> Track
                applyToSinglePointByIndex index changingTrack =
                    -- repeated use of prepareTrackPoints looks costly but check logic first.
                    Maybe.map
                        (BendSmoother.softenSinglePoint numSegments changingTrack)
                        (List.Extra.getAt index changingTrack.trackPoints)
                        |> Maybe.withDefault changingTrack

                newTrack =
                    List.foldl
                        applyToSinglePointByIndex
                        track
                        indicesToSmooth
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesNodePosition
                newTrack
                ("Autofix "
                    ++ (String.fromInt <| List.length trackPoints)
                    ++ " points."
                )
            )


deriveProblems : Track -> Options -> TrackObservations
deriveProblems track options =
    let
        suddenGradientChanges =
            List.filter
                (.gradientChange
                    >> Maybe.withDefault 0.0
                    >> (\x -> x > options.gradientChangeThreshold)
                )
                track.trackPoints

        suddenBearingChanges =
            List.filter
                (.directionChange
                    >> Maybe.withDefault Quantity.zero
                    >> Quantity.abs
                    >> Quantity.greaterThan options.directionChangeThreshold
                )
                track.trackPoints

        zeroLengths =
            List.map2
                (\pt1 pt2 ->
                    if pt1.distanceFromStart == pt2.distanceFromStart then
                        Just pt1

                    else
                        Nothing
                )
                track.trackPoints
                (List.drop 1 track.trackPoints)
                |> List.filterMap identity

        ( firstPoint, lastPoint ) =
            ( List.head track.trackPoints, List.Extra.last track.trackPoints )

        loopy =
            case ( firstPoint, lastPoint ) of
                ( Just ptStart, Just ptEnd ) ->
                    let
                        gap =
                            Point3d.distanceFrom ptStart.xyz ptEnd.xyz

                        heightDiff =
                            Point3d.zCoordinate ptStart.xyz
                                |> Quantity.minus (Point3d.zCoordinate ptEnd.xyz)
                                |> Quantity.abs
                    in
                    if
                        (gap |> Quantity.lessThanOrEqualTo (meters 1.0))
                            && (heightDiff |> Quantity.lessThanOrEqualTo (meters 1.0))
                    then
                        IsALoop

                    else if gap |> Quantity.lessThanOrEqualTo (meters 1000.0) then
                        AlmostLoop gap

                    else
                        NotALoop gap

                _ ->
                    NotALoop <| meters 0.0

        trackLength =
            track.trackPoints
                |> List.Extra.last
                |> Maybe.map .distanceFromStart
                |> Maybe.withDefault (Length.meters 0.0)
                |> inMeters

        highest =
            justXYZ
                |> List.map (Point3d.zCoordinate >> inMeters)
                |> List.maximum
                |> Maybe.withDefault 0.0

        lowest =
            justXYZ
                |> List.map (Point3d.zCoordinate >> inMeters)
                |> List.minimum
                |> Maybe.withDefault 0.0

        justXYZ =
            List.map .xyz track.trackPoints

        upVectors =
            List.filter
                (.roadVector >> Vector3d.zComponent >> Quantity.greaterThan zero)
                track.trackPoints

        downVectors =
            List.filter
                (.roadVector >> Vector3d.zComponent >> Quantity.lessThan zero)
                track.trackPoints

        ascent =
            upVectors
                |> List.map (.roadVector >> Vector3d.zComponent >> inMeters)
                |> List.sum

        descent =
            downVectors
                |> List.map (.roadVector >> Vector3d.zComponent >> inMeters)
                |> List.sum
                |> abs

        climbingDistance =
            upVectors
                |> List.map (.roadVector >> Vector3d.length >> inMeters)
                |> List.sum

        descendingDistance =
            downVectors
                |> List.map (.roadVector >> Vector3d.length >> inMeters)
                |> List.sum
    in
    { abruptGradientChanges = suddenGradientChanges
    , abruptBearingChanges = suddenBearingChanges
    , zeroLengths = zeroLengths
    , loopiness = loopy
    , trackLength = trackLength
    , highestMetres = highest
    , lowestMetres = lowest
    , climbingDistance = climbingDistance
    , descendingDistance = descendingDistance
    , totalClimbing = ascent
    , totalDescending = descent
    , meanSpacing = trackLength / (toFloat <| List.length track.trackPoints)
    }


overviewSummary : TrackObservations -> Element msg
overviewSummary obs =
    wrappedRow [ spacing 10, padding 10, width fill ]
        [ showLabelledValues
            [ ( "Highest point", showDecimal2 <| obs.highestMetres )
            , ( "Lowest point", showDecimal2 <| obs.lowestMetres )
            ]
        , showLabelledValues
            [ ( "Elevation gain ",  showDecimal2 <| obs.climbingDistance )
            , ( "Elevation loss ",  showDecimal2 <| obs.descendingDistance )
            ]
        , showLabelledValues
            [ ( "Climbing distance ",  showDecimal2 <| obs.trackLength )
            , ( "Descending distance ", showDecimal2 <| obs.totalClimbing )
            ]
        ]


viewSteepClimbs : Options -> (Msg -> msg) -> Track -> Element msg
viewSteepClimbs options wrap track =
    let
        exceeds b a =
            a > b

        exceedingThreshold =
            track.trackPoints
                |> List.filter
                    (\pt ->
                        (pt.length |> Quantity.greaterThan Quantity.zero)
                            && (gradientFromPoint pt
                                    |> exceeds options.gradientThreshold
                               )
                    )

        linkButton point =
            button prettyButtonStyles
                { onPress = Just (wrap <| LocateProblem point)
                , label = text <| showDecimal0 <| inMeters point.distanceFromStart
                }
    in
    column [ spacing 5, padding 10 ]
        [ gradientThresholdSlider options wrap
        , wrappedRow [ spacing 5, padding 10, width fill, alignLeft ] <|
            List.map linkButton exceedingThreshold
        ]


viewGradientChanges : Options -> TrackObservations -> (Msg -> msg) -> Element msg
viewGradientChanges options obs wrap =
    let
        exceeds b a =
            a > b

        exceedingThreshold =
            obs.abruptGradientChanges
                |> List.filter
                    (\pt ->
                        Maybe.withDefault 0.0 pt.gradientChange
                            |> exceeds options.gradientChangeThreshold
                    )

        linkButton point =
            button prettyButtonStyles
                { onPress = Just (wrap <| LocateProblem point)
                , label = text <| showDecimal0 <| inMeters point.distanceFromStart
                }

        autosmoothButton =
            case exceedingThreshold of
                [] ->
                    none

                _ ->
                    button prettyButtonStyles
                        { onPress = Just (wrap <| Autofix exceedingThreshold)
                        , label = text "Smooth these points in 3D"
                        }
    in
    wrappedRow [ spacing 10, padding 10 ] <|
        [ gradientChangeThresholdSlider options wrap
        , autosmoothButton
        ]
            ++ List.map linkButton exceedingThreshold


viewBearingChanges : Options -> TrackObservations -> (Msg -> msg) -> Element msg
viewBearingChanges options obs wrap =
    let
        exceeds b a =
            a > b

        exceedingThreshold =
            obs.abruptBearingChanges
                |> List.filter
                    (\pt ->
                        Maybe.withDefault zero pt.directionChange
                            |> Quantity.greaterThan options.directionChangeThreshold
                    )

        linkButton point =
            button prettyButtonStyles
                { onPress = Just (wrap <| LocateProblem point)
                , label = text <| showDecimal0 <| inMeters point.distanceFromStart
                }

        autosmoothButton =
            case exceedingThreshold of
                [] ->
                    none

                _ ->
                    button prettyButtonStyles
                        { onPress = Just (wrap <| Autofix exceedingThreshold)
                        , label = text "Smooth these points in 3D"
                        }
    in
    wrappedRow [ spacing 10, padding 10 ] <|
        [ bearingChangeThresholdSlider options wrap
        , autosmoothButton
        ]
            ++ List.map linkButton exceedingThreshold


gradientThresholdSlider : Options -> (Msg -> msg) -> Element msg
gradientThresholdSlider options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetGradientThreshold
        , label =
            Input.labelBelow [] <|
                text <|
                    "Show climbs of "
                        ++ showDecimal0 options.gradientThreshold
                        ++ "% or more"
        , min = 10.0
        , max = 30.0
        , step = Just 1.0
        , value = options.gradientThreshold
        , thumb = Input.defaultThumb
        }


gradientChangeThresholdSlider : Options -> (Msg -> msg) -> Element msg
gradientChangeThresholdSlider options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetGradientChangeThreshold
        , label =
            Input.labelBelow [] <|
                text <|
                    "Threshold = "
                        ++ showDecimal0 options.gradientChangeThreshold
                        ++ "%"
        , min = 5.0
        , max = 20.0
        , step = Just 1.0
        , value = options.gradientChangeThreshold
        , thumb = Input.defaultThumb
        }


bearingChangeThresholdSlider : Options -> (Msg -> msg) -> Element msg
bearingChangeThresholdSlider options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBearingChangeThreshold << Angle.degrees
        , label =
            Input.labelBelow [] <|
                text <|
                    "Threshold = "
                        ++ showDecimal0 (Angle.inDegrees options.directionChangeThreshold)
                        ++ "º"
        , min = 20.0
        , max = 120.0
        , step = Just 1.0
        , value = Angle.inDegrees options.directionChangeThreshold
        , thumb = Input.defaultThumb
        }
