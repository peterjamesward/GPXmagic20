module RotateRoute exposing (..)

import Angle exposing (Angle)
import Axis3d
import ColourPalette exposing (scrollbarBackground)
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d exposing (LineSegment3d)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Point3d
import PostUpdateActions
import Quantity
import SketchPlane3d
import Track exposing (Track)
import TrackEditType as PostUpdateActions
import TrackPoint exposing (TrackPoint, prepareTrackPoints, trackPointFromPoint)
import Utils exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles, wideSliderStyles)


info =
    """## Rotate Route

Spin the entire route around its centre by an arbitrary angle.

Please note angles are positive ANTI-clockwise.

"Now't as queer as folk."
"""


type Msg
    = RotateRoute
    | SetRotateAngle Angle
    | Recentre


type alias Options =
    { rotateAngle : Angle
    }


defaultOptions : Options
defaultOptions =
    { rotateAngle = Angle.degrees 0
    }


update :
    Msg
    -> Options
    -> ( Float, Float )
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction msg )
update msg settings lastMapClick track =
    case msg of
        SetRotateAngle theta ->
            ( { settings | rotateAngle = theta }
            , PostUpdateActions.ActionNoOp
            )

        RotateRoute ->
            let
                ( newTrack, undoMsg ) =
                    rotateRoute settings track
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                newTrack
                undoMsg
            )

        Recentre ->
            let
                ( newTrack, undoMsg ) =
                    recentre settings lastMapClick track
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                newTrack
                undoMsg
            )


rotateRoute : Options -> Track -> ( Track, String )
rotateRoute settings track =
    let
        undoMessage =
            "rotate by "
                ++ (showDecimal0 <| Angle.inDegrees settings.rotateAngle)

        rotatedRoute =
            List.map rotatePoint track.trackPoints

        rotatePoint =
            .xyz
                >> Point3d.rotateAround Axis3d.z settings.rotateAngle
                >> trackPointFromPoint
    in
    ( { track | trackPoints = rotatedRoute }
    , undoMessage
    )


recentre : Options -> ( Float, Float ) -> Track -> ( Track, String )
recentre settings ( lon, lat ) track =
    ( { track | earthReferenceCoordinates = ( lon, lat, 0.0 ) }
    , "recentre"
    )



view : Options -> (Msg -> msg) -> Track -> Element msg
view options wrapper track =
    let
        rotationSlider =
            Input.slider
                wideSliderStyles
                { onChange = wrapper << SetRotateAngle << Angle.degrees
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Rotation: "
                                ++ (showDecimal0 <| Angle.inDegrees options.rotateAngle)
                , min = -180.0
                , max = 180.0
                , step = Just 1.0
                , value = Angle.inDegrees <| options.rotateAngle
                , thumb = Input.defaultThumb
                }

        ( lon, lat, _ ) =
            track.earthReferenceCoordinates
    in
    column [ spacing 5, padding 5, centerX ]
        [ rotationSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| RotateRoute
            , label =
                text <|
                    "Rotate"
            }
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| Recentre
            , label =
                text <|
                    "Recentre at\n("
                        ++ String.fromFloat lon
                        ++ ", "
                        ++ String.fromFloat lat
                        ++ ")"
            }
        ]
