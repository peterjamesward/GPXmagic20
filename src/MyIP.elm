module MyIP exposing (processIpInfo, requestIpInformation, sendIpInfo)

import GeoCodeDecoders exposing (IpInfo, encodeIpInfo, encodeLogInfo, ipInfoDecoder)
import Http exposing (header)
import Iso8601
import Json.Encode
import M3O exposing (m3O_API_TOKEN)
import Time exposing (Posix)
import Url.Builder as Builder



{-
    http://ip-api.com/json/

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


apiRoot =
    "http://ip-api.com"


loggerRoot =
    "https://api.m3o.com"


requestIpInformation : (Result Http.Error IpInfo -> msg) -> Cmd msg
requestIpInformation msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Builder.crossOrigin apiRoot [ "json" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg ipInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


processIpInfo : Result Http.Error IpInfo -> Maybe IpInfo
processIpInfo response =
    case response of
        Ok ipInfo ->
            Just ipInfo

        Err _ ->
            Nothing


sendIpInfo : Posix -> (Result Http.Error () -> msg) -> Maybe IpInfo -> Cmd msg
sendIpInfo time msg ipInfo =
    case ipInfo of
        Just info ->
            let
                logInfo =
                    { timestamp = String.left 10 <| Iso8601.fromTime time
                    , ip = info.ip
                    , country = info.country
                    , region = info.region
                    , city = info.city
                    , zip = info.zip
                    , latitude = info.latitude
                    , longitude = info.longitude
                    }
            in
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ m3O_API_TOKEN)
                    ]
                , url = Builder.crossOrigin loggerRoot [ "v1", "db", "Create" ] []
                , body = Http.jsonBody <| encodeLogInfo logInfo
                , expect = Http.expectWhatever msg
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none



{-
   curl "https://api.m3o.com/v1/db/Create" \
   2-H "Content-Type: application/json" \
   3-H "Authorization: Bearer $M3O_API_TOKEN" \
   4-d '{
   5  "record": {
   6    "age": 42,
   7    "id": "1",
   8    "isActive": true,
   9    "name": "Jane"
   10  },
   11  "table": "users"
   12}'
-}
