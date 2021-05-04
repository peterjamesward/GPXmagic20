module GeoCodeDecoders exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Encode



{- Example from http://ip-api.com/json/

    {
       "status": "success",
       "country": "United Kingdom",
       "countryCode": "GB",
       "region": "ENG",
       "regionName": "England",
       "city": "Stanmore",
       "zip": "HA7",
       "lat": 51.6167,
       "lon": -0.3167,
       "timezone": "Europe/London",
       "isp": "BT Public Internet Service",
       "org": "",
       "as": "AS2856 British Telecommunications PLC",
       "query": "109.147.206.253"
   }
-}


type alias IpInfo =
    { ip : String
    , country : String
    , region : String
    , city : String
    , zip : String
    , latitude : Float
    , longitude : Float
    }

type alias LogInfo =
    { timestamp : String -- YYYY-MM-DD, so we only store each IP daily.
    , ip : String
    , country : String
    , region : String
    , city : String
    , zip : String
    , latitude : Float
    , longitude : Float
    }

ipInfoDecoder : Decoder IpInfo
ipInfoDecoder =
    D.map7 IpInfo
        (D.at [ "query" ] D.string)
        (D.at [ "country" ] D.string)
        (D.at [ "region" ] D.string)
        (D.at [ "city" ] D.string)
        (D.at [ "zip" ] D.string)
        (D.at [ "lat" ] D.float)
        (D.at [ "lon" ] D.float)


encodeIpInfo : IpInfo -> Json.Encode.Value
encodeIpInfo record =
    Json.Encode.object
        [ ( "ip", Json.Encode.string <| record.ip )
        , ( "country", Json.Encode.string <| record.country )
        , ( "region", Json.Encode.string <| record.region )
        , ( "city", Json.Encode.string <| record.city )
        , ( "zip", Json.Encode.string <| record.zip )
        , ( "lat", Json.Encode.float <| record.latitude )
        , ( "lon", Json.Encode.float <| record.longitude )
        ]

encodeLogInfo : LogInfo -> Json.Encode.Value
encodeLogInfo record =
    Json.Encode.object
        [ ( "timestamp", Json.Encode.string <| record.timestamp )
        , ( "ip", Json.Encode.string <| record.ip )
        , ( "country", Json.Encode.string <| record.country )
        , ( "region", Json.Encode.string <| record.region )
        , ( "city", Json.Encode.string <| record.city )
        , ( "zip", Json.Encode.string <| record.zip )
        , ( "lat", Json.Encode.float <| record.latitude )
        , ( "lon", Json.Encode.float <| record.longitude )
        ]
