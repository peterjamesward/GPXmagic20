module Flythrough exposing (..)

import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import FeatherIcons
import Length exposing (inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import PostUpdateActions exposing (PostUpdateAction)
import Time
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)
import Utils exposing (showDecimal0, useIcon)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Flythrough

One of the original motivations for GPXmagic, this tools
lets you fast-forward an automated ride along the route.

All linked views will update as the flythrough proceeds.

Note that the rewind button returns to the point where you
started, not the start of the route."""


type Msg
    = SetFlythroughSpeed Float
    | StartFlythrough
    | PauseFlythrough
    | ResetFlythrough


type alias Flythrough =
    { cameraPosition : Point3d Length.Meters LocalCoords
    , focusPoint : Point3d Length.Meters LocalCoords
    , metresFromRouteStart : Float
    , pointsRemaining : List TrackPoint -- (could be a smart move O Pete)
    , lastUpdated : Time.Posix
    , running : Bool
    }


type alias Options =
    { flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    , modelTime : Time.Posix
    }


defaultOptions =
    { flythroughSpeed = 1.0
    , flythrough = Nothing
    , modelTime = Time.millisToPosix 0
    }


eyeHeight =
    2.0


flythrough :
    Time.Posix
    -> Flythrough
    -> Float
    -> Flythrough
flythrough newTime flying speed =
    -- Change for V2; we maintain our own track point list,
    -- being the "tail" of the whole track.
    -- I hope, but don't overly care, that this does not do a deep copy.
    let
        tempus =
            toFloat (Time.posixToMillis newTime - Time.posixToMillis flying.lastUpdated) / 1000.0

        newDistance =
            flying.metresFromRouteStart + tempus * 10.0 ^ speed

        remainingPoints =
            case flying.pointsRemaining of
                pointBehind :: pointInFront :: _ ->
                    if newDistance >= inMeters pointInFront.distanceFromStart then
                        List.drop 1 flying.pointsRemaining

                    else
                        flying.pointsRemaining

                _ ->
                    -- Not goood.
                    flying.pointsRemaining
    in
    if not flying.running then
        flying

    else
        case remainingPoints of
            pointBehind :: pointInFront :: pointsBeyond ->
                let
                    segInsetMetres =
                        newDistance - inMeters pointBehind.distanceFromStart

                    segLength =
                        inMeters pointInFront.distanceFromStart
                            - inMeters pointBehind.distanceFromStart

                    segFraction =
                        segInsetMetres / segLength

                    segRemaining =
                        segLength - segInsetMetres

                    headTurnFraction =
                        -- Allow for POV rotation as we near segment end.
                        clamp 0.0 1.0 (10.0 - segRemaining) / 10.0

                    camera3d =
                        -- The camera is where the bike is!
                        Point3d.translateBy
                            (Vector3d.meters 0.0 0.0 eyeHeight)
                        <|
                            Point3d.interpolateFrom
                                pointBehind.xyz
                                pointInFront.xyz
                                segFraction

                    lookingAt =
                        -- Should be looking at the next point, until we are close
                        -- enough to start looking at the one beyond that.
                        case pointsBeyond of
                            pointBeyond :: _ ->
                                Point3d.interpolateFrom
                                    pointInFront.xyz
                                    pointBeyond.xyz
                                    headTurnFraction
                                    |> Point3d.translateBy
                                        (Vector3d.meters 0.0 0.0 eyeHeight)

                            [] ->
                                Point3d.translateBy
                                    (Vector3d.meters 0.0 0.0 eyeHeight)
                                    pointInFront.xyz
                in
                { flying
                    | metresFromRouteStart = newDistance
                    , lastUpdated = newTime
                    , cameraPosition = camera3d
                    , focusPoint = lookingAt
                    , pointsRemaining = remainingPoints
                }

            _ ->
                { flying | running = False }


flythroughControls : Options -> (Msg -> msg) -> Element msg
flythroughControls options wrapper =
    let
        flythroughSpeedSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetFlythroughSpeed
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Fly-through speed = "
                                ++ showDecimal0 (10.0 ^ options.flythroughSpeed)
                                ++ " m/sec"
                , min = 1.0 -- i.e. 1
                , max = 3.0 -- i.e. 1000
                , step = Nothing
                , value = options.flythroughSpeed
                , thumb = Input.defaultThumb
                }

        resetButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper ResetFlythrough
                , label = useIcon FeatherIcons.rewind
                }

        playButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper StartFlythrough
                , label = useIcon FeatherIcons.play
                }

        pauseButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper <| PauseFlythrough
                , label = useIcon FeatherIcons.pause
                }

        playPauseButton =
            case options.flythrough of
                Nothing ->
                    playButton

                Just flying ->
                    if flying.running then
                        pauseButton

                    else
                        playButton
    in
    column [ padding 10, spacing 10, centerX ]
        [ row [ padding 10, spacing 10, centerX ]
            [ resetButton
            , playPauseButton
            , flythroughSpeedSlider
            ]
        , case options.flythrough of
            Just flying ->
                row [ spacing 10 ]
                    [ text "Metres from start "
                    , text <| showDecimal0 flying.metresFromRouteStart
                    ]

            Nothing ->
                none
        ]


update : Options -> Msg -> (Msg -> msg) -> Track -> ( Options, PostUpdateAction )
update options msg wrap track =
    case msg of
        SetFlythroughSpeed speed ->
            ( { options | flythroughSpeed = speed }
            , PostUpdateActions.ActionNoOp
            )

        StartFlythrough ->
            ( startFlythrough track options
            , PostUpdateActions.ActionNoOp
            )

        PauseFlythrough ->
            ( togglePause options
            , PostUpdateActions.ActionNoOp
            )

        ResetFlythrough ->
            ( { options | flythrough = resetFlythrough track options }
            , PostUpdateActions.ActionNoOp
            )



--TODO: Use track orientation to place camera??


resetFlythrough : Track -> Options -> Maybe Flythrough
resetFlythrough track options =
    case List.drop track.currentNode.index track.track of
        pt1 :: pt2 :: rest ->
            let
                focusPoint =
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 eyeHeight)
                        pt2.xyz

                cameraShift =
                    Vector3d.from pt1.xyz pt2.xyz
                        |> Vector3d.reverse
                        |> Vector3d.scaleTo (meters 10.0)
            in
            Just
                { metresFromRouteStart = inMeters track.currentNode.distanceFromStart
                , pointsRemaining = pt1 :: pt2 :: rest
                , running = False
                , cameraPosition =
                    focusPoint
                        |> Point3d.translateBy
                            cameraShift
                , focusPoint = focusPoint
                , lastUpdated = options.modelTime
                }

        _ ->
            -- Hmm, not enough track
            Nothing


startFlythrough : Track -> Options -> Options
startFlythrough track options =
    case resetFlythrough track options of
        Just flying ->
            { options | flythrough = Just { flying | running = True } }

        Nothing ->
            options


togglePause : Options -> Options
togglePause options =
    case options.flythrough of
        Just flying ->
            { options
                | flythrough =
                    Just
                        { flying
                            | running = not flying.running
                        }
            }

        Nothing ->
            options


advanceFlythrough : Time.Posix -> Options -> Options
advanceFlythrough newTime options =
    case options.flythrough of
        Just flying ->
            { options
                | flythrough =
                    Just <|
                        flythrough
                            newTime
                            flying
                            options.flythroughSpeed
            }

        Nothing ->
            options
