module MapBox exposing (..)

import Accordion exposing (AccordionEntry)
import DisplayOptions
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
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
import ScenePainterCommon exposing (ImageMsg(..))
import Styles.Outdoors
import Styles.Satellite
import Track exposing (Track, mapboxMarkerJSON)
import ViewingContext exposing (ViewingContext)


mapGrey =
    E.makeRGBColor (E.float 150) (E.float 150) (E.float 150)


mapOrange =
    E.makeRGBColor (E.float 255) (E.float 150) (E.float 0)


mapPurple =
    E.makeRGBColor (E.float 128) (E.float 0) (E.float 128)


buildMap : List (AccordionEntry model msg) -> ViewingContext -> Track -> DisplayOptions.MapStyle -> Style
buildMap accordion context track mapStyle =
    let
        geojson =
            Track.mapboxJSON track

        trackSource =
            Source.geoJSONFromValue "track" [] geojson

        orangeSource =
            Source.geoJSONFromValue "orange" [] <| mapboxMarkerJSON track track.currentNode

        purpleSource =
            case track.markedNode of
                Just marker ->
                    Just <| Source.geoJSONFromValue "purple" [] <| mapboxMarkerJSON track marker

                Nothing ->
                    Nothing

        draggingSource =
            case context.mapDrag of
                Just drag ->
                    Just <| Source.geoJSONFromValue "drag" [] <| mapboxMarkerJSON track drag

                Nothing ->
                    Nothing

        trackLayer =
            Layer.line "track"
                "track"
                [ Layer.lineColor mapGrey, Layer.lineWidth <| E.float 2 ]

        pointsLayer =
            Layer.circle "points"
                "track"
                [ Layer.circleColor mapGrey, Layer.circleRadius <| E.float 4 ]

        orangeMarkerLayer =
            Layer.circle "orange"
                "orange"
                [ Layer.circleColor mapOrange, Layer.circleRadius <| E.float 8 ]

        purpleMarkerLayer =
            case purpleSource of
                Just purple ->
                    Just <|
                        Layer.circle "purple"
                            "purple"
                            [ Layer.circleColor mapPurple, Layer.circleRadius <| E.float 6 ]

                Nothing ->
                    Nothing

        draggingLayer =
            case context.mapDrag of
                Just drag ->
                    Just <|
                        Layer.circle "drag"
                            "drag"
                            [ Layer.circleRadius <| E.float 8 ]

                Nothing ->
                    Nothing

        optionalSources =
            [ purpleSource, draggingSource ] |> List.filterMap identity

        optionalLayers =
            [ purpleMarkerLayer, draggingLayer ] |> List.filterMap identity

        baseStyle =
            case mapStyle of
                DisplayOptions.MapSatellite ->
                    Styles.Satellite.style

                _ ->
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
                    | sources = orangeSource :: trackSource :: optionalSources ++ base.sources
                    , layers =
                        -- Note: they are drawn in this order.
                        base.layers
                            ++ [ trackLayer, pointsLayer, orangeMarkerLayer ]
                            ++ optionalLayers
                    , misc = trackCentre :: trackZoom :: base.misc
                }

        FromUrl string ->
            baseStyle


view : ViewingContext -> Style -> (ImageMsg -> msg) -> Html msg
view context trackStyle wrap =
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
            [ maxZoom 20
            , minZoom 1
            , token MapboxKey.mapboxKey
            , onClick (wrap << MapClick)
            , onMouseMove (wrap << MapMouseMove)
            , onDblClick (wrap << MapDoubleClick)
            , id "my-map"
            , eventFeaturesLayers [ "points" ]
            ]
            trackStyle
        ]


centreMapOn : ( Float, Float ) -> Cmd msg
centreMapOn ( lon, lat ) =
    let
        lngLat =
            { lng = lon, lat = lat }
    in
    MapCommands.jumpTo [ Opt.center lngLat ]


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
