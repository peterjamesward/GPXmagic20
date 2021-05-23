module Track exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d
import EarthConstants exposing (metresPerDegree)
import Element exposing (..)
import GpxParser
import Graph exposing (Graph)
import Json.Encode as E
import Length exposing (Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import SketchPlane3d
import Spherical
import TrackPoint exposing (TrackPoint, applyGhanianTransform, prepareTrackPoints)
import Utils exposing (bearingToDisplayDegrees, showDecimal2, showDecimal6)
import Vector3d exposing (..)


type alias Track =
    { trackPoints : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , earthReferenceCoordinates : ( Float, Float, Float ) -- (lon, lat, ele)
    , box : BoundingBox3d Meters LocalCoords
    }


trackFromGpx : String -> Maybe Track
trackFromGpx content =
    let
        trackPoints =
            GpxParser.parseTrackPoints content

        ( lons, lats ) =
            List.map (\( lon, lat, _ ) -> ( lon, lat )) trackPoints |> List.unzip

        lonExtrema =
            ( List.minimum lons, List.maximum lons )

        latExtrema =
            ( List.minimum lats, List.maximum lats )

        basePoint =
            ( case lonExtrema of
                ( Just minLon, Just maxLon ) ->
                    (minLon + maxLon) / 2.0

                _ ->
                    0.0
            , case latExtrema of
                ( Just minLat, Just maxLat ) ->
                    (minLat + maxLat) / 2.0

                _ ->
                    0.0
            , 0.0
            )

        centredPoints =
            -- Move to near (0,0) to maintain precision in geometry -> clip space
            applyGhanianTransform basePoint trackPoints
    in
    case centredPoints of
        [] ->
            Nothing

        n1 :: _ ->
            Just
                { trackName = GpxParser.parseTrackName content
                , trackPoints = prepareTrackPoints centredPoints
                , currentNode = n1
                , markedNode = Nothing
                , graph = Nothing
                , earthReferenceCoordinates = basePoint
                , box =
                    BoundingBox3d.hullOfN .xyz centredPoints
                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
                }


removeGhanianTransform : Track -> List ( Float, Float, Float )
removeGhanianTransform track =
    List.map
        (.xyz >> withoutGhanianTransform track)
        track.trackPoints


withoutGhanianTransform : Track -> Point3d Meters LocalCoords -> ( Float, Float, Float )
withoutGhanianTransform track point =
    let
        ( baseLon, baseLat, baseEle ) =
            -- Local points are angular distances scaled by latitude of base point
            track.earthReferenceCoordinates

        ( x, y, z ) =
            Point3d.toTuple inMeters point
    in
    ( x / metresPerDegree / cos (degrees baseLat) + baseLon
    , y / metresPerDegree + baseLat
    , z
    )


latLonPair ( lon, lat, ele ) =
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
    List.Extra.getAt (from.index + 1) track.trackPoints
        |> Maybe.withDefault from


prevPointOn : Track -> TrackPoint -> TrackPoint
prevPointOn track from =
    List.Extra.getAt (from.index - 1) track.trackPoints
        |> Maybe.withDefault from


summaryData : Track -> Element msg
summaryData track =
    let
        pt =
            track.currentNode

        gradient =
            100.0
                * (Vector3d.zComponent pt.roadVector |> inMeters)
                / (pt.length |> inMeters)

        ( lat, lon ) =
            pt.latLon
    in
    column [ centerX ]
        [ row [ padding 20, centerX, spacing 10 ]
            [ column [ spacing 10 ]
                [ text "Start point index "
                , text "Length "
                ]
            , column [ spacing 10 ]
                [ text <| String.fromInt pt.index
                , text <| showDecimal2 <| inMeters <| pt.length
                ]
            , column [ spacing 10 ]
                [ text "Gradient "
                , text "Bearing "
                ]
            , column [ spacing 10 ]
                [ text <| showDecimal2 gradient
                , text <|
                    bearingToDisplayDegrees <|
                        Maybe.map (Direction3d.azimuthIn SketchPlane3d.xy) <|
                            pt.afterDirection
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
                [ text <| showDecimal6 <| Angle.inDegrees lat
                , text <| showDecimal6 <| Angle.inDegrees lon
                , text <| showDecimal2 <| inMeters <| Point3d.zCoordinate pt.xyz
                , text <| showDecimal2 <| inMeters pt.distanceFromStart
                ]
            ]
        ]


searchTrackPointFromLonLat : ( Float, Float ) -> Track -> Maybe TrackPoint
searchTrackPointFromLonLat ( lon, lat ) track =
    let
        trackLonLats =
            List.Extra.zip
                (removeGhanianTransform track)
                (List.range 0 (List.length track.trackPoints))

        searchLatLon =
            ( Angle.degrees lat, Angle.degrees lon )

        nearestPair =
            trackLonLats
                |> List.Extra.minimumBy
                    (\( ( lon1, lat1, _ ), _ ) -> Spherical.range ( Angle.degrees lat1, Angle.degrees lon1 ) searchLatLon)
    in
    case nearestPair of
        Just ( _, index ) ->
            List.Extra.getAt index track.trackPoints

        _ ->
            Nothing


updateTrackPointLonLat : ( Float, Float ) -> Track -> TrackPoint -> TrackPoint
updateTrackPointLonLat ( lon, lat ) track tp =
    let
        transformedLonLats =
            applyGhanianTransform track.earthReferenceCoordinates [ ( lon, lat, 0.0 ) ]
    in
    case transformedLonLats of
        newLocation :: _ ->
            let
                ele =
                    Point3d.zCoordinate tp.xyz |> inMeters

                ( x, y, z ) =
                    Point3d.toTuple inMeters newLocation.xyz

                newXYZ =
                    Point3d.fromTuple Length.meters ( x, y, ele )
            in
            { tp | xyz = newXYZ }

        _ ->
            tp


trackBoundingBox : Track -> BoundingBox3d Meters LocalCoords
trackBoundingBox track =
    BoundingBox3d.hullOfN .xyz track.trackPoints
        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
