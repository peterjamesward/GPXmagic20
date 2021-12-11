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
            [ maxZoom 5

            --, onMouseMove Hover
            --, onClick Click
            , id "my-map"
            , eventFeaturesLayers [ "changes" ]
            , hoveredFeatures []
            ]
            (Style
                { transition = Style.defaultTransition
                , light = Style.defaultLight
                , sources =
                    [ Source.vectorFromUrl "composite"
                        "mapbox://mapbox.mapbox-terrain-v2,mapbox.mapbox-streets-v7,astrosat.07pz1g3y"
                    , Source.geoJSONFromValue "changes" [] geojson
                    ]
                , misc =
                    [ Style.name "light"
                    , Style.defaultCenter <| LngLat 20.39789404164037 43.22523201923144
                    , Style.defaultZoomLevel 1.5967483759772743
                    , Style.sprite "mapbox://sprites/mapbox/light"
                    , Style.glyphs "mapbox://fonts/mapbox/{fontstack}/{range}.pbf"
                    ]
                , layers =
                    [ Layer.background "background"
                        [ E.rgba 246 246 244 1 |> Layer.backgroundColor
                        ]
                    , Layer.fill "landcover"
                        "composite"
                        [ Layer.sourceLayer "landcover"
                        , E.any
                            [ E.getProperty (str "class") |> E.isEqual (str "wood")
                            , E.getProperty (str "class") |> E.isEqual (str "scrub")
                            , E.getProperty (str "class") |> E.isEqual (str "grass")
                            , E.getProperty (str "class") |> E.isEqual (str "crop")
                            ]
                            |> Layer.filter
                        , Layer.fillColor (E.rgba 227 227 227 1)
                        , Layer.fillOpacity (float 0.6)
                        ]
                    , Layer.symbol "place-city-lg-n"
                        "composite"
                        [ Layer.sourceLayer "place_label"
                        , Layer.minzoom 1
                        , Layer.maxzoom 14
                        , Layer.filter <|
                            E.all
                                [ E.getProperty (str "scalerank") |> E.greaterThan (int 2)
                                , E.getProperty (str "type") |> E.isEqual (str "city")
                                ]
                        , Layer.textField <|
                            E.format
                                [ E.getProperty (str "name_en")
                                    |> E.formatted
                                    |> E.fontScaledBy (float 1.2)
                                , E.formatted (str "\n")
                                , E.getProperty (str "name")
                                    |> E.formatted
                                    |> E.fontScaledBy (float 0.8)
                                    |> E.withFont (E.strings [ "DIN Offc Pro Medium" ])
                                ]
                        ]
                    , Layer.fill "changes"
                        "changes"
                        [ Layer.fillOpacity (E.ifElse (E.toBool (E.featureState (str "hover"))) (float 0.9) (float 0.1))
                        ]
                    ]
                }
            )
        ]
