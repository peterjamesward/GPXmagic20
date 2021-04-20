module Track exposing (..)

import Graph exposing (Graph)
import Json.Encode as E
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Vector3d exposing (Vector3d)


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , transform : Vector3d Meters LocalCoords
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

        latLonPair tp =
            let
                ( lon, lat, ele ) =
                    pointInEarthCoordinates tp
            in
            E.list E.float [ lon, lat ]
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

        latLonPair tp =
            let
                ( lon, lat, ele ) =
                    pointInEarthCoordinates tp
            in
            E.list E.float [ lon, lat ]
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]
