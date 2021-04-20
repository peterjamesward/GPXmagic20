port module MapController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Element exposing (Element, centerX, column, padding, row, spacing, text)
import Json.Decode exposing (Decoder, decodeValue, field, float, string)
import Json.Encode as E
import Length
import LocalCoords exposing (LocalCoords)
import MapboxKey exposing (mapboxKey)
import Point3d
import TrackPoint exposing (TrackPoint, trackPointsToJSON, trackToJSON)
import Utils exposing (showDecimal2, showDecimal6)


type MapStyle
    = MapStyleStreets
    | MapStyleOutdoors
    | MapStyleSatellite


type MapState
    = WaitingForNode
    | WaitingForMapLoad
    | MapLoaded
    | TrackLoaded
    | MapStopping
    | MapStopped


type
    MapMessage
    -- These will arrive through the inbound Port and update the state machine.
    = NodeAvailable
    | MapHasLoaded
    | TrackHasLoaded
    | MapHasStopped


type alias MapInfo =
    { mapState : MapState
    , box : BoundingBox3d Length.Meters LocalCoords
    , points : List TrackPoint
    , centreLon : Float -- track values from user map interactions.
    , centreLat : Float -- track values from user map interactions.
    , mapZoom : Float -- track values from user map interactions.
    , current : ( Float, Float ) -- orange cone
    , marker : Maybe ( Float, Float ) -- purple cone
    }


defaultMapInfo =
    { mapState = MapLoaded
    , box = BoundingBox3d.singleton Point3d.origin
    , points = []
    , centreLon = 0.0
    , centreLat = 0.0
    , mapZoom = 12.0
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


centreMap : Float -> Float -> Cmd msg
centreMap lon lat =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            ]


removeMap : Cmd msg
removeMap =
    mapPort <| E.object [ ( "Cmd", E.string "Stop" ) ]


toggleDragging : Bool -> MapInfo -> Cmd msg
toggleDragging state info =
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Drag" )
            , ( "Enable", E.bool state )
            , ( "points", trackPointsToJSON info.points ) -- Make track points draggable
            ]


addTrackToMap : MapInfo -> Cmd msg
addTrackToMap info =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable wotsits.
    mapPort <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            , ( "data", trackToJSON info.points ) -- Route as polyline
            , ( "points", trackPointsToJSON info.points ) -- Make track points draggable
            ]


addMarkersToMap :
    ( Float, Float ) -- current track point
    -> Maybe ( Float, Float ) -- dropped marker
    -> List TrackPoint -- bend smoothing suggestion
    -> List TrackPoint -- node nudging preview
    -> Cmd msg
addMarkersToMap current marker smoothBend nudged =
    let
        encodePos ( lon, lat ) =
            E.object
                [ ( "lon", E.float lon )
                , ( "lat", E.float lat )
                ]
    in
    mapPort <|
        case marker of
            Just mark ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos current )
                    , ( "purple", encodePos mark )
                    , ( "bend", trackToJSON smoothBend )
                    , ( "nudge", trackToJSON nudged )
                    ]

            Nothing ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos current )
                    , ( "bend", trackToJSON smoothBend )
                    , ( "nudge", trackToJSON nudged )
                    ]


decodeState state =
    text <|
        case state of
            WaitingForNode ->
                "Waiting for node"

            WaitingForMapLoad ->
                "Waiting for map to load"

            MapLoaded ->
                "Map loaded"

            TrackLoaded ->
                "Track loaded"

            MapStopping ->
                "Stopping"

            MapStopped ->
                "Stopped"


viewMapInfo : Maybe MapInfo -> Element msg
viewMapInfo mapInfo =
    case mapInfo of
        Just info ->
            column [ padding 10, spacing 10, centerX ]
                [ decodeState info.mapState
                , row [ padding 10, spacing 10, centerX ]
                    [ column [ padding 10, spacing 10, centerX ]
                        [ text "Longitude "
                        , text "Latitude "
                        , text "Zoom "
                        ]
                    , column [ padding 10, spacing 10, centerX ]
                        [ text <| showDecimal6 info.centreLon
                        , text <| showDecimal6 info.centreLat
                        , text <| showDecimal2 info.mapZoom
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
        Ok "no node" ->
            Just
                ( { info | mapState = WaitingForNode }
                , createMap info
                )

        Ok "map ready" ->
            Just
                ( { info | mapState = MapLoaded }
                , addTrackToMap info
                )

        Ok "track ready" ->
            Just
                ( { info | mapState = MapLoaded }
                , addMarkersToMap info.current info.marker [] []
                )

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

        _ ->
            Nothing


msgDecoder : Decoder String
msgDecoder =
    field "msg" string
