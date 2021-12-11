module MapBox exposing (..)

import Html exposing (div, text)
import Html.Attributes exposing (style)
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Cmd.Option as Opt
import Mapbox.Element exposing (..)
import Mapbox.Expression as E exposing (false, float, int, str, true)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))
import Styles.Outdoors


geojson =
    Json.Decode.decodeString Json.Decode.value """
{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "id": 1,
      "properties": {
        "name": "Bermuda Triangle",
        "area": 1150180
      },
      "geometry": {
        "type": "Polygon",
        "coordinates": [
          [
            [-64.73, 32.31],
            [-80.19, 25.76],
            [-66.09, 18.43],
            [-64.73, 32.31]
          ]
        ]
      }
    }
  ]
}
""" |> Result.withDefault (Json.Encode.object [])


hoveredFeatures : List Json.Encode.Value -> MapboxAttr msg
hoveredFeatures =
    List.map (\feat -> ( feat, [ ( "hover", Json.Encode.bool True ) ] ))
        >> featureState


view _ =
    div [ style "width" "100vw", style "height" "100vh" ]
        [ map
            [ maxZoom 20

            --, onMouseMove Hover
            --, onClick Click
            , id "my-map"
            , eventFeaturesLayers [ "changes" ]
            , hoveredFeatures []
            ]
            Styles.Outdoors.style
        ]
