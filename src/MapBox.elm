module MapBox exposing (..)

import Angle exposing (Angle)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode
import Json.Encode
import LngLat exposing (LngLat)
import MapCommands
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


buildMap : Track -> Style
buildMap track =
    let
        geojson =
            Track.mapboxJSON track

        trackSource =
            --Source.geoJSONFromValue "changes" [] testgeojson
            Source.geoJSONFromValue "track" [] geojson

        trackLayer =
            Layer.line "track" "track" []

        pointsLayer =
            Layer.circle "points" "track" []

        baseStyle =
            Styles.Outdoors.style

        ( lon, lat, _ ) =
            track.earthReferenceCoordinates

        trackCentre =
            defaultCenter { lng = lon, lat = lat }

        trackZoom =
            defaultZoomLevel 12
    in
    case baseStyle of
        Style base ->
            Style
                { base
                    | sources = trackSource :: base.sources
                    , layers = base.layers ++ [ trackLayer, pointsLayer ]
                    , misc = trackCentre :: trackZoom :: base.misc
                }

        FromUrl string ->
            baseStyle


view : ViewingContext -> Style -> Html msg
view context trackStyle =
    let
        ( width, height ) =
            context.size

        ( w, h ) =
            ( (width |> Pixels.inPixels |> String.fromInt) ++ "px"
            , (height |> Pixels.inPixels |> String.fromInt) ++ "px"
            )
    in
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
            trackStyle
        ]


centreMapOn : ( Float, Float ) -> Cmd msg
centreMapOn ( lon, lat ) =
    let
        lngLat =
            { lng = lon, lat = lat }
    in
    MapCommands.flyTo [ Opt.center lngLat ]


resizeMap : Cmd msg
resizeMap =
    MapCommands.resize


zoomIn : Cmd msg
zoomIn =
    MapCommands.zoomIn []


zoomOut : Cmd msg
zoomOut =
    MapCommands.zoomOut []


zoomReset : Float -> Cmd msg
zoomReset level =
    MapCommands.zoomTo [] level
