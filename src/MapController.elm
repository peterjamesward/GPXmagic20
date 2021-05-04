port module MapController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Delay
import Element exposing (Element, centerX, column, padding, row, spacing, text)
import Json.Decode exposing (Decoder, decodeValue, field, float, string)
import Json.Encode as E
import Length exposing (inMeters)
import LocalCoords exposing (LocalCoords)
import MapboxKey exposing (mapboxKey)
import Point3d
import Track exposing (Track, trackPointsToJSON, trackToJSON, withoutGhanianTransform)
import TrackPoint exposing (TrackPoint, pointInEarthCoordinates)
import Utils exposing (showDecimal0, showDecimal2, showDecimal6)
import ViewingContext exposing (ViewingContext)


type MapStyle
    = MapStyleStreets
    | MapStyleOutdoors
    | MapStyleSatellite


type alias MapInfo =
    { box : BoundingBox3d Length.Meters LocalCoords
    , points : List TrackPoint
    , centreLon : Float -- track values from user map interactions.
    , centreLat : Float -- track values from user map interactions.
    , mapZoom : Float -- track values from user map interactions.
    , current : ( Float, Float ) -- orange cone
    , marker : Maybe ( Float, Float ) -- purple cone
    }


defaultMapInfo =
    { box = BoundingBox3d.singleton Point3d.origin
    , points = []
    , centreLon = 0.0
    , centreLat = 0.0
    , mapZoom = 0.0
    , current = ( 0.0, 0.0 )
    , marker = Nothing
    }


port mapPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


port mapStopped : (String -> msg) -> Sub msg


createMap : MapInfo -> Cmd msg
createMap info =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


refreshMap : Cmd msg
refreshMap =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Repaint" )
            , ( "token", E.string mapboxKey )
            ]


centreMap : ViewingContext -> Track -> Cmd msg
centreMap context track =
    let
        ( lon, lat, _ ) =
            context.focalPoint
                |> withoutGhanianTransform track
                |> pointInEarthCoordinates
    in
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            ]


zoomMap : ViewingContext -> Cmd msg
zoomMap context =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Zoom" )
            , ( "token", E.string mapboxKey )
            , ( "zoom", E.float context.zoomLevel )
            ]


centreMapOnCurrent : Track -> Cmd msg
centreMapOnCurrent track =
    let
        ( lon, lat, _ ) =
            track.currentNode.xyz
                |> withoutGhanianTransform track
                |> pointInEarthCoordinates
    in
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            ]



--toggleDragging : Bool -> MapInfo -> Cmd msg
--toggleDragging state info =
--    mapPort <|
--        E.object
--            [ ( "Cmd", E.string "Drag" )
--            , ( "Enable", E.bool state )
--            , ( "points", trackPointsToJSON info.points ) -- Make track points draggable
--            ]
--
--addTrackToMap : Maybe Track -> Cmd msg


addTrackToMap : ViewingContext -> Track -> Cmd msg
addTrackToMap context track =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable wotsits.
    let
        ( x, y, z ) =
            context.focalPoint
                |> withoutGhanianTransform track
                |> pointInEarthCoordinates
    in
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float x )
            , ( "lat", E.float y )
            , ( "zoom", E.float context.zoomLevel )
            , ( "data", trackToJSON track ) -- Route as polyline
            , ( "points", trackPointsToJSON track ) -- Make track points draggable
            ]


addMarkersToMap :
    Track
    -> List TrackPoint -- bend smoothing suggestion
    -> List TrackPoint -- node nudging preview
    -> Cmd msg
addMarkersToMap track smoothBend nudged =
    let
        realWorldPosition tp =
            Track.withoutGhanianTransform track tp.xyz
                |> TrackPoint.pointInEarthCoordinates

        encodePos ( lon, lat, ele ) =
            E.object
                [ ( "lon", E.float lon )
                , ( "lat", E.float lat )
                ]
    in
    mapPort <|
        case track.markedNode of
            Just mark ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| realWorldPosition track.currentNode )
                    , ( "purple", encodePos <| realWorldPosition mark )

                    --, ( "bend", trackToJSON smoothBend )
                    --, ( "nudge", trackToJSON nudged )
                    ]

            Nothing ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| realWorldPosition track.currentNode )

                    --, ( "bend", trackToJSON smoothBend )
                    --, ( "nudge", trackToJSON nudged )
                    ]


viewMapInfo : Maybe MapInfo -> Element msg
viewMapInfo mapInfo =
    case mapInfo of
        Just info ->
            column [ padding 10, spacing 10, centerX ]
                [ row [ padding 10, spacing 10, centerX ]
                    [ column [ padding 10, spacing 10, centerX ]
                        [ text "Longitude "
                        , text "Latitude "
                        , text "Zoom "
                        ]
                    , column [ padding 10, spacing 10, centerX ]
                        [ text <| showDecimal6 info.centreLon
                        , text <| showDecimal6 info.centreLat
                        , text <| showDecimal0 info.mapZoom
                        ]
                    ]
                ]

        Nothing ->
            column [ padding 10, spacing 10, centerX ]
                [ text "Map information is available only once a map has been loaded." ]


processMapMessage : MapInfo -> E.Value -> Maybe ( MapInfo, Cmd msg )
processMapMessage info json =
    -- If we return Nothing, it means we're not interested and Main should handle it.
    let
        msg =
            decodeValue msgDecoder json
    in
    case msg of
        Ok "move" ->
            -- User is dragging/zooming the map
            --( { 'msg' : 'move'
            --  , 'lat' : map.getCentre().lat
            --  , 'lon' : map.getCentre().lon
            --  , 'zoom' : map.getZoom()
            --  } );
            let
                lat =
                    decodeValue (field "lat" float) json

                lon =
                    decodeValue (field "lon" float) json

                zoom =
                    decodeValue (field "zoom" float) json
            in
            case ( lat, lon, zoom ) of
                ( Ok lat1, Ok lon1, Ok zoom1 ) ->
                    Just
                        ( { info
                            | centreLon = lon1
                            , centreLat = lat1
                            , mapZoom = zoom1
                          }
                        , Cmd.none
                        )

                _ ->
                    Just ( info, Cmd.none )

        Ok "no node" ->
            Just
                ( info
                , createMap info
                )


        _ ->
            Nothing


msgDecoder : Decoder String
msgDecoder =
    field "msg" string
