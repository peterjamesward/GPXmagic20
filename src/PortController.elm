port module PortController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import Length
import LocalCoords exposing (LocalCoords)
import MapboxKey exposing (mapboxKey)
import Point3d
import Track exposing (Track, trackPointsToJSON, trackToJSON, withoutGhanianTransform)
import TrackPoint exposing (TrackPoint)
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


port commandPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


createMap : MapInfo -> Cmd msg
createMap info =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


refreshMap : Cmd msg
refreshMap =
    commandPort <|
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
    in
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            ]


zoomMap : ViewingContext -> Cmd msg
zoomMap context =
    commandPort <|
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
    in
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            ]


prepareSketchMap : ( Float, Float ) -> Cmd msg
prepareSketchMap ( lon, lat ) =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Sketch" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float lon )
            , ( "lat", E.float lat )
            , ( "zoom", E.float 10.0 )
            ]


exitSketchMode : Cmd msg
exitSketchMode =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "ExitSketch" )
            , ( "token", E.string mapboxKey )
            ]


toggleDragging : Bool -> Track -> Cmd msg
toggleDragging isDragging track =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Drag" )
            , ( "Enable", E.bool isDragging )
            , ( "points", trackPointsToJSON track ) -- Make track points draggable
            ]


requestElevations : Cmd msg
requestElevations =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Elev" )
            ]


addTrackToMap : ViewingContext -> Track -> Cmd msg
addTrackToMap context track =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable wotsits.
    let
        ( x, y, z ) =
            context.focalPoint
                |> withoutGhanianTransform track
    in
    commandPort <|
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
    -> Track -- bend smoothing suggestion
    -> Track -- node nudging preview
    -> Cmd msg
addMarkersToMap track smoothBend nudged =
    let
        realWorldPosition tp =
            Track.withoutGhanianTransform track tp.xyz

        encodePos ( lon, lat, ele ) =
            E.object
                [ ( "lon", E.float lon )
                , ( "lat", E.float lat )
                ]
    in
    commandPort <|
        case track.markedNode of
            Just mark ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| realWorldPosition track.currentNode )
                    , ( "purple", encodePos <| realWorldPosition mark )
                    , ( "bend", trackToJSON smoothBend )
                    , ( "nudge", trackToJSON nudged )
                    ]

            Nothing ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| realWorldPosition track.currentNode )
                    , ( "bend", trackToJSON smoothBend )
                    , ( "nudge", trackToJSON nudged )
                    ]


storageSetItem : String -> E.Value -> Cmd msg
storageSetItem key value =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.set" )
            , ( "key", E.string key )
            , ( "value", value )
            ]


storageGetItem : String -> Cmd msg
storageGetItem key =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.get" )
            , ( "key", E.string key )
            ]


storageListKeys : Cmd msg
storageListKeys =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.list" )
            ]


storageClear : Cmd msg
storageClear =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.clear" )
            ]


msgDecoder : Decoder String
msgDecoder =
    field "msg" string
