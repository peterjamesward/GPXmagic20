module Track exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d
import Element exposing (..)
import GpxParser
import Graph exposing (Graph)
import Json.Encode as E
import Length exposing (Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity
import SketchPlane3d
import TrackPoint exposing (TrackPoint, applyGhanianTransform, pointInEarthCoordinates, prepareTrackPoints)
import Utils exposing (bearingToDisplayDegrees, showDecimal2, showDecimal6)
import Vector3d exposing (Vector3d)


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , transform : Vector3d Meters LocalCoords
    , box : BoundingBox3d Meters LocalCoords
    }


trackFromGpx : String -> Maybe Track
trackFromGpx content =
    let
        trackPoints =
            GpxParser.parseTrackPoints content

        ( centredPoints, transform ) =
            -- Move to near (0,0) to maintain precision in geometry -> clip space
            applyGhanianTransform trackPoints
    in
    case centredPoints of
        [] ->
            Nothing

        n1 :: _ ->
            Just
                { trackName = GpxParser.parseTrackName content
                , track = prepareTrackPoints centredPoints
                , currentNode = n1
                , markedNode = Nothing
                , graph = Nothing
                , transform = transform
                , box =
                    BoundingBox3d.hullOfN .xyz centredPoints
                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
                }


removeGhanianTransform : Track -> List (Point3d Meters LocalCoords)
removeGhanianTransform track =
    track.track
        |> List.map .xyz
        |> List.map
            (track.transform |> Vector3d.reverse |> Point3d.translateBy)


withoutGhanianTransform : Track -> Point3d Meters LocalCoords -> Point3d Meters LocalCoords
withoutGhanianTransform track point =
    Point3d.translateBy (track.transform |> Vector3d.reverse) point


latLonPair tp =
    let
        ( lon, lat, ele ) =
            pointInEarthCoordinates tp
    in
    E.list E.float [ lon, lat ]


trackPointsToJSON : Track -> E.Value
trackPointsToJSON track =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        features =
            List.map makeFeature (removeGhanianTransform track)

        makeFeature tp =
            E.object
                [ ( "type", E.string "Feature" )
                , ( "geometry", point tp )
                ]

        point tp =
            E.object
                [ ( "type", E.string "Point" )
                , ( "coordinates", latLonPair tp )
                ]
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


trackToJSON : Track -> E.Value
trackToJSON track =
    -- JSON suitable for Mapbox API to add polyline for route.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            List.map latLonPair (removeGhanianTransform track)
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


nextPointOn : Track -> TrackPoint -> TrackPoint
nextPointOn track from =
    List.Extra.getAt (from.index + 1) track.track
        |> Maybe.withDefault from


prevPointOn : Track -> TrackPoint -> TrackPoint
prevPointOn track from =
    List.Extra.getAt (from.index - 1) track.track
        |> Maybe.withDefault from


summaryData : Track -> Element msg
summaryData track =
    let
        pt =
            track.currentNode

        gradient =
            pt.roadVector
                |> Vector3d.direction
                |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
                |> Maybe.withDefault Quantity.zero
                |> Angle.tan
                |> (*) 100.0

        bearing =
            pt.afterDirection
                |> Maybe.map (Direction3d.azimuthIn SketchPlane3d.xy)
                |> Maybe.withDefault Quantity.zero

        ( lon, lat, ele ) =
            pt.xyz
                |> withoutGhanianTransform track
                |> pointInEarthCoordinates
    in
    column [ centerX ]
        [ row [ padding 20, centerX, spacing 10 ]
            [ column [ spacing 10 ]
                [ text "Start point index "
                , text "Length "
                ]
            , column [ spacing 10 ]
                [ text <| String.fromInt pt.index
                , text <| showDecimal2 <| inMeters <| Vector3d.length pt.roadVector
                ]
            , column [ spacing 10 ]
                [ text "Gradient "
                , text "Bearing "
                ]
            , column [ spacing 10 ]
                [ text <| showDecimal2 gradient
                , text <| bearingToDisplayDegrees bearing
                ]
            ]
        , row [ padding 10, centerX, alignTop, spacing 10 ]
            [ column [ spacing 10 ]
                [ text "Latitude "
                , text "Longitude "
                , text "Elevation "
                , text "Distance "
                ]
            , column [ spacing 10 ]
                [ text <| showDecimal6 lat
                , text <| showDecimal6 lon
                , text <| showDecimal2 <| inMeters <| Point3d.zCoordinate pt.xyz
                , text <| showDecimal2 <| inMeters pt.distanceFromStart
                ]
            ]
        ]
