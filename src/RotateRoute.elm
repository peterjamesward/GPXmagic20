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

        RotateRoute ->
            -- As of 2.8, does rotate & scale
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildRotateAndScale settings track)
            )

        Recentre ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildRecentre settings lastMapClick track)
            )

        ScaleRoute ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                PostUpdateActions.EditPreservesIndex
                (buildRescale settings track)
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


buildRotate : Options -> Track -> UndoEntry
buildRotate settings track =
    { label = "Rotate by " ++ (showDecimal0 <| Angle.inDegrees settings.rotateAngle)
    , editFunction = rotateRoute settings
    , undoFunction = rotateRoute { settings | rotateAngle = Quantity.negate settings.rotateAngle }
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


rotateRoute : Options -> Track -> EditResult
rotateRoute settings track =
    -- Gotta like how this serves for undo as well.
    let
        rotatedRoute =
            List.map rotatePoint track.trackPoints

        axisOfRotation =
            Axis3d.through track.currentNode.xyz Direction3d.z

        rotatePoint =
            .xyz
                >> Point3d.rotateAround axisOfRotation settings.rotateAngle
                >> trackPointFromPoint
    in
    { before = []
    , edited = rotatedRoute
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


buildRecentre : Options -> ( Float, Float ) -> Track -> UndoEntry
buildRecentre settings ( lon, lat ) track =
    { label = "Recentre"
    , editFunction = recentre settings ( lon, lat, 0.0 )
    , undoFunction = recentre settings track.earthReferenceCoordinates
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


recentre : Options -> ( Float, Float, Float ) -> Track -> EditResult
recentre settings ( lon, lat, _ ) track =
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
    }


buildRescale : Options -> Track -> UndoEntry
buildRescale settings track =
    { label = "Rescale"
    , editFunction = rescale settings
    , undoFunction = rescale { settings | scaleFactor = 1.0 / settings.scaleFactor }
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


buildRotateAndScale : Options -> Track -> UndoEntry
buildRotateAndScale settings track =
    { label = "Rotate & Scale"
    , editFunction = editAsTrack (rescale settings) >> rotateRoute settings
    , undoFunction =
        editAsTrack (rescale { settings | scaleFactor = 1.0 / settings.scaleFactor })
            >> rotateRoute { settings | rotateAngle = Quantity.negate settings.rotateAngle }
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


rescale : Options -> Track -> EditResult
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
                (.xyz >> scaleAboutCentre >> trackPointFromPoint)
                track.trackPoints
    in
    { before = []
    , edited = scaledPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    }


buildFullTransform : Options -> ( Float, Float ) -> Track -> UndoEntry
buildFullTransform settings ( lon, lat ) track =
    -- Wow.
    { label = "Preview"
    , editFunction =
        editAsTrack (recentre settings ( lon, lat, 0.0 ))
            >> editAsTrack (rescale settings)
            >> rotateRoute settings
    , undoFunction =
        editAsTrack (rotateRoute { settings | rotateAngle = Quantity.negate settings.rotateAngle })
            >> editAsTrack (rescale { settings | scaleFactor = 1.0 / settings.scaleFactor })
            >> recentre settings track.earthReferenceCoordinates
    , newOrange = track.currentNode.index
    , newPurple = Maybe.map .index track.markedNode
    , oldOrange = track.currentNode.index
    , oldPurple = Maybe.map .index track.markedNode
    }


getPreview3D : Options -> ( Float, Float ) -> Track -> List (Entity LocalCoords)
getPreview3D options lastMapClick track =
    let
        actions =
            buildFullTransform options lastMapClick track

        result =
            actions.editFunction track
    in
    highlightPoints Color.lightGreen result.edited


getPreviewMap : Options -> ( Float, Float ) -> Track -> E.Value
getPreviewMap options lastMapClick track =
    let
        {-
           To return JSON:
           { "name" : "nudge"
           , "colour" : "#FFFFFF"
           , "points" : <trackPointsToJSON ...>
           }
        -}
        actions =
            buildFullTransform options lastMapClick track

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
                { onPress = Just <| wrapper RotateRoute
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
        [ rotationSlider
        , scaleSlider
        , rotateButton
        , recentreButton
        , elevationFetchButton
        ]
