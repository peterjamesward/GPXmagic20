module Track exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import GpxParser
import Graph exposing (Graph)
import Json.Encode as E
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import TrackPoint exposing (TrackPoint, applyGhanianTransform, pointInEarthCoordinates, prepareTrackPoints)
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


