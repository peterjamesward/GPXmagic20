module Track exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import ColourPalette exposing (scrollbarBackground)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import FeatherIcons
import GpxParser
import Graph exposing (Graph)
import Json.Encode as E
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import TrackPoint exposing (TrackPoint, applyGhanianTransform, pointInEarthCoordinates, prepareTrackPoints)
import Utils exposing (scrollbarThickness, useIcon)
import Vector3d exposing (Vector3d)
import ViewPureStyles exposing (prettyButtonStyles)


type Msg
    = PositionForwardOne
    | PositionBackOne
    | PositionSlider Int


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , transform : Vector3d Meters LocalCoords
    , box : BoundingBox3d Meters LocalCoords
    }


trackFromGpx : String -> Maybe Track
trackFromGpx content =
    let
        trackPoints =
            GpxParser.parseTrackPoints content

        ( centredPoints, transform ) =
            -- Move to near (0,0) to maintain precision in geometry -> clip space
            applyGhanianTransform trackPoints
    in
    case centredPoints of
        [] ->
            Nothing

        n1 :: _ ->
            Just
                { trackName = GpxParser.parseTrackName content
                , track = prepareTrackPoints centredPoints
                , currentNode = n1
                , markedNode = Nothing
                , graph = Nothing
                , transform = transform
                , box =
                    BoundingBox3d.hullOfN .xyz centredPoints
                        |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)
                }


removeGhanianTransform : Track -> List (Point3d Meters LocalCoords)
removeGhanianTransform track =
    track.track
        |> List.map .xyz
        |> List.map
            (track.transform |> Vector3d.reverse |> Point3d.translateBy)


withoutGhanianTransform : Track -> Point3d Meters LocalCoords -> Point3d Meters LocalCoords
withoutGhanianTransform track point =
    Point3d.translateBy (track.transform |> Vector3d.reverse) point


latLonPair tp =
    let
        ( lon, lat, ele ) =
            pointInEarthCoordinates tp
    in
    E.list E.float [ lon, lat ]


trackPointsToJSON : Track -> E.Value
trackPointsToJSON track =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        features =
            List.map makeFeature (removeGhanianTransform track)

        makeFeature tp =
            E.object
                [ ( "type", E.string "Feature" )
                , ( "geometry", point tp )
                ]

        point tp =
            E.object
                [ ( "type", E.string "Point" )
                , ( "coordinates", latLonPair tp )
                ]
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


trackToJSON : Track -> E.Value
trackToJSON track =
    -- JSON suitable for Mapbox API to add polyline for route.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            List.map latLonPair (removeGhanianTransform track)
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


viewTrackControls : (Msg -> msg) -> Maybe Track -> Element msg
viewTrackControls wrap track =
    Maybe.map (positionControls wrap) track |> Maybe.withDefault none


positionControls : (Msg -> msg) -> Track -> Element msg
positionControls wrap track =
    row
        [ spacing 5
        , padding 5
        , centerX
        , centerY
        ]
        [ positionSlider wrap track
        , button
            prettyButtonStyles
            { onPress = Just <| wrap PositionBackOne
            , label = useIcon FeatherIcons.skipBack
            }
        , button
            prettyButtonStyles
            { onPress = Just <| wrap PositionForwardOne
            , label = useIcon FeatherIcons.skipForward
            }
        ]


positionSlider : (Msg -> msg) -> Track -> Element msg
positionSlider wrap track =
    Input.slider
        [ height <| px scrollbarThickness
        , width <| px 300
        , centerY
        , behindContent <|
            -- Slider track
            el
                [ width <| px 300
                , height <| px scrollbarThickness
                , centerY
                , centerX
                , Background.color scrollbarBackground
                , Border.rounded 6
                ]
                none
        ]
        { onChange = wrap << (PositionSlider << round)
        , label =
            Input.labelHidden "Drag slider or use arrow buttons"
        , min = 0.0
        , max = toFloat <| List.length track.track - 1
        , step = Just 1
        , value = toFloat track.currentNode.index
        , thumb = Input.defaultThumb
        }


update : Msg -> Track -> Track
update msg track =
    let
        safeNewNode newIndex =
            case List.Extra.getAt newIndex track.track of
                Just tp ->
                    { track | currentNode = tp }

                Nothing ->
                    track
    in
    case msg of
        PositionForwardOne ->
            safeNewNode <| track.currentNode.index + 1

        PositionBackOne ->
            safeNewNode <| track.currentNode.index - 1

        PositionSlider index ->
            safeNewNode index
