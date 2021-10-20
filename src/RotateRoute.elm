module RotateRoute exposing (..)

import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
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
import Utils exposing (showDecimal0, showDecimal2, showLongMeasure)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles, wideSliderStyles)


info =
    """## Lift & Shift

Rotate Route will turn the entire route around its centre by an arbitrary angle.
(Please note angles are positive ANTI-clockwise.)

Recentre will move the nominal centre of the route to the LAST PLACE CLICKED on Map View.

Rescale will linearly increase the route dimensions by the scale factor you set.

"""


type Msg
    = RotateRoute
    | SetRotateAngle Angle
    | Recentre
    | SetScale Float
    | ScaleRoute
    | UseMapElevations


type alias Options =
    { rotateAngle : Angle
    , scaleFactor : Float
    }


defaultOptions : Options
defaultOptions =
    { rotateAngle = Angle.degrees 0
    , scaleFactor = 1.0
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

        SetScale scale ->
            ( { settings | scaleFactor = scale }
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

        ScaleRoute ->
            let
                ( newTrack, undoMsg ) =
                    rescale settings track
            in
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                newTrack
                undoMsg
            )

        UseMapElevations ->
            ( settings
            , PostUpdateActions.ActionFetchMapElevations
            )


applyMapElevations : List Float -> Track -> Track
applyMapElevations elevations track =
    let
        useNewElevation tp ele =
            Point3d.xyz
                (Point3d.xCoordinate tp.xyz)
                (Point3d.yCoordinate tp.xyz)
                (Length.meters ele)
                |> TrackPoint.trackPointFromPoint

        newPoints =
            List.map2
                useNewElevation
                track.trackPoints
                elevations
                |> TrackPoint.prepareTrackPoints
    in
    { track
        | trackPoints = newPoints
        , box =
            BoundingBox3d.hullOfN .xyz newPoints
                |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
    }


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


rescale : Options -> Track -> ( Track, String )
rescale settings track =
    let
        centre =
            Point3d.xyz
                (BoundingBox3d.midX track.box)
                (BoundingBox3d.midY track.box)
                (BoundingBox3d.minZ track.box)

        scaleAboutCentre point =
            centre
                |> Point3d.translateBy
                    (point |> Vector3d.from centre |> Vector3d.scaleBy settings.scaleFactor)

        scaledPoints =
            List.map
                (.xyz >> scaleAboutCentre)
                track.trackPoints

        scaledTrack =
            { track
                | trackPoints =
                    List.map TrackPoint.trackPointFromPoint scaledPoints
                        |> TrackPoint.prepareTrackPoints
                , box =
                    BoundingBox3d.hullN scaledPoints
                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
            }
    in
    ( scaledTrack
    , "rescale"
    )


view : Bool -> Options -> (Float, Float) -> (Msg -> msg) -> Track -> Element msg
view imperial options (lastX, lastY) wrapper track =
    let
        rotationSlider =
            Input.slider
                commonShortHorizontalSliderStyles
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

        scaleSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetScale << (\x -> 10.0 ^ x)
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Scale: "
                                ++ showDecimal2 options.scaleFactor
                , min = -1.0
                , max = 1.0
                , step = Nothing
                , value = logBase 10 options.scaleFactor
                , thumb = Input.defaultThumb
                }

        ( lon, lat, _ ) =
            track.earthReferenceCoordinates

        trackLength =
            case List.Extra.last track.trackPoints of
                Just last ->
                    inMeters last.distanceFromStart

                _ ->
                    0.0

        rotateButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper RotateRoute
                , label =
                    text <|
                        "Rotate"
                }

        recentreButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper Recentre
                , label =
                    text <|
                        "Recentre at\n("
                            ++ String.fromFloat lastX
                            ++ ", "
                            ++ String.fromFloat lastY
                            ++ ")"
                }

        scaleButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper ScaleRoute
                , label =
                    text <|
                        "Scale track to "
                            ++ showLongMeasure imperial (Length.meters <| options.scaleFactor * trackLength)
                }

        elevationFetchButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper UseMapElevations
                , label = text "Use elevations fetched from Mapbox"
                }
    in
    wrappedRow [ spacing 5, padding 5 ]
        [ column [ spacing 5, padding 5 ] [ rotationSlider, rotateButton ]
        , column [ spacing 5, padding 5 ] [ scaleSlider, scaleButton ]
        , recentreButton
        , elevationFetchButton
        ]
