----------------------------------------------------------------------
--
-- LocalStorage.elm
-- Elm interface to JavaScript's localStorage facility
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------
-- Modified by PJW to not use PortFunnel. Because.


port module LocalStorage exposing
    ( clear
    , get
    , listKeys
    , put
    , subPort
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


port cmdPort : JE.Value -> Cmd msg


port subPort : (JE.Value -> msg) -> Sub msg


{-| A `Response` is used to return values for `Get` and `ListKeys`.
-}
type Response
    = NoResponse
    | GetResponse { key : String, value : Maybe Value }
    | ListKeysResponse { keys : List String }


{-| An opaque type that represents a message to send to or receive from the JS code.

There are a number of internal messages, but the ones you can use are created by `get`,
`put`, `listkeys`, `clear`, and `useSessionStorage`.

-}
type StorageMessage
    = Get
    | Put String (Maybe JE.Value)
    | ListKeys
    | Clear


get : String -> StorageMessage
get k =
    Get


put : String -> Maybe Value -> StorageMessage
put k v =
    Put k v


listKeys : StorageMessage
listKeys =
    ListKeys


clear : StorageMessage
clear =
    Clear


encodeLabeledString : Maybe String -> String -> String -> Value
encodeLabeledString label string property =
    JE.object
        [ ( "label"
          , case label of
                Just lab ->
                    JE.string lab

                Nothing ->
                    JE.null
          )
        , ( property, JE.string string )
        ]


labeledStringDecoder : String -> Decoder ( Label, String )
labeledStringDecoder property =
    JD.map2 Tuple.pair
        (JD.field "label" <| JD.nullable JD.string)
        (JD.field property JD.string)


type alias GotRecord =
    { key : String
    , value : Value
    }


gotDecoder : Decoder GotRecord
gotDecoder =
    JD.map2 GotRecord
        (JD.field "key" JD.string)
        (JD.field "value" JD.value)


type alias PutRecord =
    { key : String
    , value : Value
    }


putDecoder : Decoder PutRecord
putDecoder =
    JD.map2 PutRecord
        (JD.field "key" JD.string)
        (JD.field "value" JD.value)


type alias KeysRecord =
    { keys : List String
    }


keysDecoder : Decoder KeysRecord
keysDecoder =
    JD.map KeysRecord
        (JD.field "keys" <| JD.list JD.string)


send : (Value -> Cmd msg) -> StorageMessage -> Cmd msg
send wrapper message =
    let
        mess =
            case message of
                Get key ->
                    Get key

                Put key value ->
                    Put key value

                ListKeys ->
                    ListKeys

                Clear ->
                    Clear

                _ ->
                    message
    in
    cmdPort mess


processIncomingMessage : StorageMessage -> Response
processIncomingMessage message =
    case message of
        Got key value ->
            GetResponse
                { key = key
                , value = value
                }

        Keys keys ->
            ListKeysResponse
                { keys = keys
                }

        _ ->
            NoResponse


toJsonString : StorageMessage -> String
toJsonString message =
        JE.encode 0 message
