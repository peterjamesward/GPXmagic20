module MapBox exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import Mapbox.Cmd.Option as Opt
import Mapbox.Element exposing (..)
import Mapbox.Expression as E exposing (false, float, int, str, true)
import Mapbox.Layer as Layer
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..), defaultCenter, defaultZoomLevel)
import MapboxKey
import Pixels
import Styles.Outdoors
import Track exposing (Track)
import ViewingContext exposing (ViewingContext)


testgeojson =
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


view : ViewingContext -> Track -> Html msg
view context track =
    let
        ( width, height ) =
            context.size

        ( w, h ) =
            ( (width |> Pixels.inPixels |> String.fromInt) ++ "px"
            , (height |> Pixels.inPixels |> String.fromInt) ++ "px"
            )

        baseStyle =
            Styles.Outdoors.style

        geojson =
            Track.mapboxJSON track

        trackSource =
            --Source.geoJSONFromValue "changes" [] testgeojson
            Source.geoJSONFromValue "track" [] geojson

        trackLayer =
            --Layer.fill "changes"
            --    "changes"
            --    [ Layer.fillOpacity (E.ifElse (E.toBool (E.featureState (str "hover"))) (float 0.9) (float 0.1))
            --    ]
            Layer.circle "track" "track" []

        ( lon, lat, _ ) =
            track.earthReferenceCoordinates

        trackCentre =
            defaultCenter { lng = lon, lat = lat }

        trackZoom =
            defaultZoomLevel context.zoomLevel
    in
    case baseStyle of
        Style base ->
            div [ style "width" w, style "height" h ]
                [ map
                    [ maxZoom 16
                    , minZoom 1
                    , token MapboxKey.mapboxKey

                    --, onMouseMove Hover
                    --, onClick Click
                    , id "my-map"
                    , eventFeaturesLayers []
                    ]
                    (Style
                        { base
                            | sources = trackSource :: base.sources
                            , layers = base.layers ++ [trackLayer]
                            , misc = trackCentre :: trackZoom :: base.misc
                        }
                    )
                ]

        FromUrl string ->
            text "Something wrong with the Map style"
