module Track exposing (..)

import Angle
import BoundingBox2d
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
import SpatialIndex
import Spherical
import TrackPoint exposing (TrackPoint, applyGhanianTransform, prepareTrackPoints)
import Utils exposing (bearingToDisplayDegrees, clickTolerance, flatBox, showDecimal2, showDecimal6, showLabelledValues, showLongMeasure, showShortMeasure)
import Vector3d exposing (..)


type alias Track =
    { trackPoints : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , earthReferenceCoordinates : ( Float, Float, Float ) -- (lon, lat, ele)
    , box : BoundingBox3d Meters LocalCoords
    , spatialIndex : SpatialIndex.SpatialNode TrackPoint Length.Meters LocalCoords
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
            let
                points =
                    prepareTrackPoints centredPoints

                box =
                    BoundingBox3d.hullOfN .xyz points
                        |> Maybe.withDefault (BoundingBox3d.singleton n1.xyz)

                emptyIndex =
                    -- Large split threshold to avoid excessive depth.
                    SpatialIndex.empty (flatBox box) (Length.meters 100.0)

                index =
                    List.foldl
                        (\point ->
                            SpatialIndex.add
                                { content = point
                                , box =
                                    BoundingBox2d.withDimensions clickTolerance
                                        (point.xyz |> Point3d.projectInto SketchPlane3d.xy)
                                }
                        )
                        emptyIndex
                        points
            in
            Just
                { trackName = GpxParser.parseTrackName content
                , trackPoints = points
                , currentNode = n1
                , markedNode = Nothing
                , graph = Nothing
                , earthReferenceCoordinates = basePoint
                , box = box
                , spatialIndex = index
                }


trackFromMap : List ( Float, Float, Float ) -> Maybe Track
trackFromMap trackPoints =
    --TODO: This is almost exactly the same as the function above.
    let
        lons =
            List.map (\( lon, lat, ele ) -> lon) trackPoints

        lats =
            List.map (\( lon, lat, ele ) -> lat) trackPoints

        eles =
            List.map (\( lon, lat, ele ) -> ele) trackPoints

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
            let
                points =
                    prepareTrackPoints centredPoints

                box =
                    BoundingBox3d.hullOfN .xyz points
                        |> Maybe.withDefault (BoundingBox3d.singleton n1.xyz)

                emptyIndex =
                    -- Large split threshold to avoid excessive depth.
                    SpatialIndex.empty (flatBox box) (Length.meters 100.0)

                index =
                    List.foldl
                        (\point ->
                            SpatialIndex.add
                                { content = point
                                , box =
                                    BoundingBox2d.withDimensions clickTolerance
                                        (point.xyz |> Point3d.projectInto SketchPlane3d.xy)
                                }
                        )
                        emptyIndex
                        points
            in
            Just
                { trackName = Just "from-sketch"
                , trackPoints = points
                , currentNode = n1
                , markedNode = Nothing
                , graph = Nothing
                , earthReferenceCoordinates = basePoint
                , box = box
                , spatialIndex = index
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


summaryData : Bool -> Track -> Element msg
summaryData imperial track =
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
    wrappedRow [ spacing 10, padding 10 ]
        [ showLabelledValues
            [ ( "Start index", String.fromInt pt.index )
            , ( "From start ", showLongMeasure imperial pt.distanceFromStart )
            ]
        , showLabelledValues
            [ ( "Length", showShortMeasure imperial pt.length )
            , ( "Gradient", showDecimal2 gradient )
            ]
        , showLabelledValues
            [ ( "Latitude ", showDecimal6 <| Angle.inDegrees lat )
            , ( "Longitude ", showDecimal6 <| Angle.inDegrees lon )
            ]
        , showLabelledValues
            [ ( "Elevation ", showShortMeasure imperial (Point3d.zCoordinate pt.xyz) )
            , ( "Bearing"
              , bearingToDisplayDegrees <|
                    Maybe.map (Direction3d.azimuthIn SketchPlane3d.xy) <|
                        pt.afterDirection
              )
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
