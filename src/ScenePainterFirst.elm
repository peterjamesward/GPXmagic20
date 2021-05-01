module ScenePainterFirst exposing (..)

import Angle
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveZ)
import DisplayOptions exposing (DisplayOptions)
import EarthConstants exposing (metresPerPixel)
import Element exposing (Element, el, html, none, row)
import Flythrough exposing (eyeHeight)
import Length exposing (meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point3d
import Quantity exposing (Quantity)
import Scene exposing (Scene)
import Scene3d exposing (backgroundColor)
import ScenePainterCommon exposing (ImageMsg, trackPointNearestRay, withMouseCapture, zoomButtons, zoomLevelFromBoundingBox)
import SketchPlane3d
import Track exposing (Track)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Vector3d
import ViewingContext exposing (ViewingContext, newViewingContext)
import ViewingMode exposing (ViewingMode(..))
import Viewpoint3d


initialiseView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Track
    -> ViewingContext
initialiseView viewSize track =
    -- This is just a simple default so we can see something!
    let
        firstPointOnTrack =
            track.track
                |> List.head
                |> Maybe.map .xyz
                |> Maybe.withDefault centralPoint
                |> Point3d.translateBy (Vector3d.meters 0 0 1)

        ( zoom, centralPoint ) =
            zoomLevelFromBoundingBox viewSize track.track

        viewContext =
            newViewingContext ViewFirstPerson

        _ = Debug.log "Azimuth" trackAzimuth

        trackAzimuth =
            track.track
                |> List.head
                |> Maybe.andThen .afterDirection
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.reverse
                |> Direction3d.azimuthIn SketchPlane3d.xy
    in
    { viewContext
        | focalPoint = firstPointOnTrack
        , azimuth = trackAzimuth
        , elevation = Angle.degrees 10
        , sceneSearcher = trackPointNearestRay track.track
        , zoomLevel = 14.0
        , defaultZoomLevel = 14.0
    }


viewScene :
    Bool
    -> ViewingContext
    -> DisplayOptions
    -> Scene
    -> (ImageMsg -> msg)
    -> Element msg
viewScene visible context options scene wrapper =
    row []
        [ if visible then
            el (withMouseCapture wrapper) <|
                html <|
                    if options.withLighting then
                        Scene3d.sunny
                            { camera = deriveViewPointAndCamera context
                            , dimensions = context.size
                            , background = backgroundColor Color.lightBlue
                            , clipDepth = Length.meters 1
                            , entities = scene
                            , upDirection = positiveZ
                            , sunlightDirection = negativeZ
                            , shadows = False
                            }

                    else
                        Scene3d.unlit
                            { camera = deriveViewPointAndCamera context
                            , dimensions = context.size
                            , background = backgroundColor Color.lightBlue
                            , clipDepth = Length.meters 1
                            , entities = scene
                            }

          else
            none
        , zoomButtons wrapper
        ]


deriveViewPointAndCamera : ViewingContext -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera view =
    let
        ( _, latitude, _ ) =
            pointInEarthCoordinates view.focalPoint

        cameraViewpoint =
            case view.flythrough of
                Nothing ->
                    Viewpoint3d.orbitZ
                        { focalPoint = view.focalPoint
                            |> Point3d.translateBy (Vector3d.meters 0 0 1)
                        , azimuth = view.azimuth
                        , elevation = view.elevation
                        , distance =
                            Length.meters <|
                                50.0
                                    * metresPerPixel view.zoomLevel (degrees latitude)
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = flying.cameraPosition
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees <| 120.0 - view.zoomLevel * 4.0
        }
