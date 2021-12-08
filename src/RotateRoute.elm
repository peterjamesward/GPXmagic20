module RotateRoute exposing (..)

import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Color
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Json.Encode as E
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point3d
import PostUpdateActions exposing (EditResult, UndoEntry, editAsTrack)
import Quantity
import Scene3d exposing (Entity)
import SceneBuilder exposing (highlightPoints)
import Track exposing (Track)
import TrackEditType
import TrackPoint exposing (TrackPoint, prepareTrackPoints, trackPointFromPoint)
import Utils exposing (showDecimal0, showDecimal2, showLongMeasure)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles, wideSliderStyles)


toolLabel =
    "Lift & Shift"


info =
    """## Lift & Shift

Rotate Route will turn the entire route around its centre by an arbitrary angle.
(Please note angles are positive ANTI-clockwise.)

Recentre will move the nominal centre of the route to the LAST PLACE CLICKED on Map View.

Rescale will linearly increase the route dimensions by the scale factor you set.

"""


type Msg
    = RotateAndScale
    | SetRotateAngle Angle
    | Recentre
    | SetScale Float
    | Zero
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
    -> ( Options, PostUpdateActions.PostUpdateAction trck cmd )
update msg settings lastMapClick track =
    case msg of
        SetRotateAngle theta ->
            ( { settings | rotateAngle = theta }
            , PostUpdateActions.ActionPreview
            )

        SetScale scale ->
            ( { settings | scaleFactor = scale }
            , PostUpdateActions.ActionPreview
            )

        RotateAndScale ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildRotateAndScale settings track)
            )

        Recentre ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildRecentre settings lastMapClick track)
            )

        Zero ->
            ( defaultOptions
            , PostUpdateActions.ActionPreview
            )

        UseMapElevations ->
            ( settings
            , PostUpdateActions.ActionFetchMapElevations
            )


buildMapElevations : List Float -> Track -> UndoEntry
buildMapElevations elevations track =
    let
        altitude tp =
            tp |> .xyz |> Point3d.zCoordinate |> Length.inMeters
    in
    { label = "Elevations from map"
    , editFunction = applyMapElevations elevations
    , undoFunction = applyMapElevations (List.map altitude track.trackPoints)
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


applyMapElevations : List Float -> Track -> EditResult
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
    { before = []
    , edited = newPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


buildRecentre : Options -> ( Float, Float ) -> Track -> UndoEntry
buildRecentre settings lastMapClick track =
    { label = "Recentre"
    , editFunction = recentre lastMapClick
    , undoFunction = undoRecentre track.earthReferenceCoordinates
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


recentre : ( Float, Float ) -> Track -> EditResult
recentre ( lon, lat ) track =
    -- To allow us to use the Purple marker as a designated reference,
    -- we need to move the earth reference coords AND shift the track
    -- by the opposite of the Purple position (?).
    let
        shiftedTrackPoints =
            List.map shiftPoint track.trackPoints

        shiftBasis =
            case track.markedNode of
                Just purple ->
                    purple.xyz |> Point3d.projectOnto Plane3d.xy

                Nothing ->
                    Point3d.origin

        shiftVector =
            Vector3d.from shiftBasis Point3d.origin

        shiftPoint =
            .xyz
                >> Point3d.translateBy shiftVector
                >> trackPointFromPoint
    in
    { before = []
    , edited = shiftedTrackPoints
    , after = []
    , earthReferenceCoordinates = ( lon, lat, 0.0 )
    , graph = track.graph
    }


undoRecentre : ( Float, Float, Float ) -> Track -> EditResult
undoRecentre ( lon, lat, alt ) track =
    -- To allow us to use the Purple marker as a designated reference,
    -- we need to move the earth reference coords AND shift the track
    -- by the opposite of the Purple position (?).
    let
        shiftedTrackPoints =
            List.map shiftPoint track.trackPoints

        shiftBasis =
            case track.markedNode of
                Just purple ->
                    purple.xyz |> Point3d.projectOnto Plane3d.xy

                Nothing ->
                    Point3d.origin

        shiftVector =
            Vector3d.from Point3d.origin shiftBasis

        shiftPoint =
            .xyz
                >> Point3d.translateBy shiftVector
                >> trackPointFromPoint
    in
    { before = []
    , edited = shiftedTrackPoints
    , after = []
    , earthReferenceCoordinates = ( lon, lat, alt )
    , graph = track.graph
    }


buildRotateAndScale : Options -> Track -> UndoEntry
buildRotateAndScale settings track =
    { label = "Rotate & Scale"
    , editFunction = rotateAndScale settings
    , undoFunction =
        rotateAndScale
            { settings
                | scaleFactor = 1.0 / settings.scaleFactor
                , rotateAngle = Quantity.negate settings.rotateAngle
            }
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


rotateAndScale : Options -> Track -> EditResult
rotateAndScale settings track =
    let
        centre =
            -- Scale about centre of bounding box
            Point3d.xyz
                (BoundingBox3d.midX track.box)
                (BoundingBox3d.midY track.box)
                (BoundingBox3d.minZ track.box)

        axisOfRotation =
            -- Rotate acts around Orange marker
            Axis3d.through track.currentNode.xyz Direction3d.z

        rotatedRoute =
            List.map rotatePoint track.trackPoints

        rotatePoint =
            .xyz
                >> Point3d.rotateAround axisOfRotation settings.rotateAngle
                >> trackPointFromPoint

        scaleAboutCentre point =
            centre
                |> Point3d.translateBy
                    (point |> Vector3d.from centre |> Vector3d.scaleBy settings.scaleFactor)

        transformedPoints =
            List.map
                (.xyz >> scaleAboutCentre >> trackPointFromPoint)
                rotatedRoute
    in
    { before = []
    , edited = transformedPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


getPreview3D : Options -> Track -> List (Entity LocalCoords)
getPreview3D options track =
    let
        actions =
            buildRotateAndScale options track

        result =
            actions.editFunction track
    in
    highlightPoints Color.lightGreen result.edited


getPreviewMap : Options -> Track -> E.Value
getPreviewMap options track =
    let
        {-
           To return JSON:
           { "name" : "nudge"
           , "colour" : "#FFFFFF"
           , "points" : <trackPointsToJSON ...>
           }
        -}
        actions =
            buildRotateAndScale options track

        results =
            actions.editFunction track

        fakeTrack =
            -- Just for the JSON
            { track | trackPoints = results.edited }
    in
    E.object
        [ ( "name", E.string "transform" )
        , ( "colour", E.string "#3BDD3B" )
        , ( "points", Track.trackToJSON fakeTrack )
        ]


view : Bool -> Options -> ( Float, Float ) -> (Msg -> msg) -> Track -> Element msg
view imperial options ( lastX, lastY ) wrapper track =
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
                , min = -30.0
                , max = 30.0
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
                { onPress = Just <| wrapper RotateAndScale
                , label =
                    text <|
                        "Rotate & Scale"
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

        zeroButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper Zero
                , label = text "Zero"
                }

        elevationFetchButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper UseMapElevations
                , label = text "Use elevations fetched from Mapbox"
                }
    in
    wrappedRow [ spacing 5, padding 5 ]
        [ rotationSlider
        , scaleSlider
        , rotateButton
        , zeroButton
        , recentreButton
        , elevationFetchButton
        ]
