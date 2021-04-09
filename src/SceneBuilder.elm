module SceneBuilder exposing (..)

import Angle
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import FlatColors.BritishPalette
import Length
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import TrackPoint exposing (TrackPoint, trackPointBearing)
import Vector3d
import Viewpoint3d


type alias Scene =
    List (Entity LocalCoords)


type alias RenderingContext =
    -- The information we need to turn a track point list into a scene.
    { detailLevel : Float
    , trackDistanceInFocus : Quantity Float Length.Meters
    }


defaultRenderingContext =
    { detailLevel = 1.0
    , trackDistanceInFocus = Quantity.zero
    }


render : RenderingContext -> List TrackPoint -> Scene
render context track =
    -- Let's just try a clean room implementation here, with surface only.
    List.concat <|
        List.map2
            paintSurfaceBetween
            track
            (List.drop 1 track)


paintSurfaceBetween : TrackPoint -> TrackPoint -> List (Entity LocalCoords)
paintSurfaceBetween pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1.xyz pt2.xyz

        halfWidth =
            Vector3d.from pt1.xyz pt2.xyz
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo (Length.meters 3.0)

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( LineSegment3d.translateBy leftKerbVector roadAsSegment
            , LineSegment3d.translateBy rightKerbVector roadAsSegment
            )
    in
    [ Scene3d.quad (Material.matte Color.grey)
        (LineSegment3d.startPoint leftKerb)
        (LineSegment3d.endPoint leftKerb)
        (LineSegment3d.endPoint rightKerb)
        (LineSegment3d.startPoint rightKerb)
    ]


setUpCamera : List TrackPoint -> Camera3d Length.Meters LocalCoords
setUpCamera track =
    let
        firstPointOnTrack =
            track
                |> List.head
                |> Maybe.map .xyz
                |> Maybe.withDefault Point3d.origin

        eyeOffset = Vector3d.meters 100.0 100.0 10.0

        viewPoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.translateBy eyeOffset firstPointOnTrack
                , focalPoint = firstPointOnTrack
                , upDirection = Direction3d.positiveZ
                }
    in
    Camera3d.perspective
        { viewpoint = viewPoint
        , verticalFieldOfView = Angle.degrees 30.0
        }
