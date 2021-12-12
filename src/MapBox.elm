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
import Mapbox.Style as Style exposing (Style(..))
import MapboxKey
import Pixels
import Styles.Outdoors
import Track exposing (Track)
import ViewingContext exposing (ViewingContext)


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
            track |> Track.trackToJSON

        trackSource =
            Source.geoJSONFromValue "track" [] geojson

        trackLayer =
            Layer.line "track"
                "track"
                [
                ]
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
                    , id ("my-map" ++ String.fromInt context.contextId)
                    , eventFeaturesLayers []
                    ]
                    (Style
                        { base
                            | sources = trackSource :: base.sources
                            , layers = trackLayer :: base.layers
                        }
                    )
                ]

        FromUrl string ->
            text "Something wrong with the Map style"
