port module PortController exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import Length
import LocalCoords exposing (LocalCoords)
import Point3d
import TrackPoint exposing (TrackPoint)



port commandPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


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
