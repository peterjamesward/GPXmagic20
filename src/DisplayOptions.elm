module DisplayOptions exposing (..)

import Element exposing (..)
import Element.Input as Input exposing (button)
import Json.Decode as Decode exposing (Decoder, bool, decodeValue, field, float, int)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, prettyButtonStyles, radioButton)


info =
    """## Display options

Change the way that the track is presented.

These will be seen mostly in the three dimensional views
as the Plan and Profile views don't benefit much.
"""


type CurtainStyle
    = NoCurtain
    | PlainCurtain
    | RainbowCurtain
    | PastelCurtain


type Msg
    = TogglePillars Bool
    | ToggleImperial Bool
    | ToggleLighting Bool
    | ToggleSeaLevel Bool
    | ToggleRoad Bool
    | ToggleCones Bool
    | ToggleCentreLine Bool
    | SetCurtainStyle CurtainStyle
    | SetVerticalExaggeration Float
    | Terrain Bool
    | TerrainFineness Int


type Measurements
    = Imperial
    | Metric


type alias DisplayOptions =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , curtainStyle : CurtainStyle
    , problems : Bool
    , centreLine : Bool
    , terrainOn : Bool
    , seaLevel : Bool
    , withLighting : Bool
    , verticalExaggeration : Float
    , terrainFineness : Int
    , imperialMeasure : Bool
    }


defaultDisplayOptions : DisplayOptions
defaultDisplayOptions =
    { roadPillars = True
    , roadCones = True
    , roadTrack = True
    , curtainStyle = PastelCurtain
    , problems = False
    , centreLine = False
    , terrainOn = False
    , seaLevel = True
    , withLighting = True
    , verticalExaggeration = 1.0
    , terrainFineness = 50
    , imperialMeasure = False
    }


viewDisplayOptions : DisplayOptions -> (Msg -> msg) -> Element msg
viewDisplayOptions options wrap =
    column [ width fill ]
        [ wrappedRow [ spacing 10, padding 10 ]
            [ Input.checkbox []
                { onChange = wrap << ToggleRoad
                , icon = checkboxIcon
                , checked = options.roadTrack
                , label = Input.labelRight [ centerY ] (text "Road surface")
                }
            , Input.checkbox []
                { onChange = wrap << ToggleCentreLine
                , icon = checkboxIcon
                , checked = options.centreLine
                , label = Input.labelRight [ centerY ] (text "Centre line")
                }
            , Input.checkbox []
                { onChange = wrap << ToggleSeaLevel
                , icon = checkboxIcon
                , checked = options.seaLevel
                , label = Input.labelRight [ centerY ] (text "Sea level")
                }
            , Input.checkbox []
                { onChange = wrap << Terrain
                , icon = checkboxIcon
                , checked = options.terrainOn
                , label = Input.labelRight [ centerY ] (text "Terrain")
                }
            , Input.checkbox []
                { onChange = wrap << TogglePillars
                , icon = checkboxIcon
                , checked = options.roadPillars
                , label = Input.labelRight [ centerY ] (text "Pillars")
                }
            , Input.checkbox []
                { onChange = wrap << ToggleCones
                , icon = checkboxIcon
                , checked = options.roadCones
                , label = Input.labelRight [ centerY ] (text "Trackpoints")
                }
            , Input.checkbox []
                { onChange = wrap << ToggleLighting
                , icon = checkboxIcon
                , checked = options.withLighting
                , label = Input.labelRight [ centerY ] (text "Lighting")
                }
            , Input.checkbox []
                { onChange = wrap << ToggleImperial
                , icon = checkboxIcon
                , checked = options.imperialMeasure
                , label = Input.labelRight [ centerY ] (text "Imperial")
                }
            , Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << TerrainFineness << round
                , label =
                    Input.labelBelow [] <| text "Terrain blockiness"
                , min = 10.0
                , max = 100.0
                , step = Just 5.0
                , value = toFloat options.terrainFineness
                , thumb = Input.defaultThumb
                }
            , Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrap << SetVerticalExaggeration
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Profile scale: "
                                ++ showDecimal2 options.verticalExaggeration
                , min = 1.0
                , max = 10.0
                , step = Nothing
                , value = options.verticalExaggeration
                , thumb = Input.defaultThumb
                }
            ]
        , Input.radioRow
            [ spaceEvenly, padding 10 ]
            { onChange = wrap << SetCurtainStyle
            , selected = Just options.curtainStyle
            , label =
                Input.labelLeft [] <| text "Gradients:"
            , options =
                [ Input.optionWith NoCurtain <| radioButton "None"
                , Input.optionWith PlainCurtain <| radioButton "Plain"
                , Input.optionWith PastelCurtain <| radioButton "Pastel"
                , Input.optionWith RainbowCurtain <| radioButton "Vivid"
                ]
            }
        ]


update : DisplayOptions -> Msg -> (Msg -> msg) -> DisplayOptions
update options dispMsg wrap =
    case dispMsg of
        TogglePillars newState ->
            { options | roadPillars = newState }

        ToggleLighting newState ->
            { options | withLighting = newState }

        ToggleSeaLevel newState ->
            { options | seaLevel = newState }

        ToggleRoad newState ->
            { options | roadTrack = newState }

        ToggleCones newState ->
            { options | roadCones = newState }

        ToggleCentreLine newState ->
            { options | centreLine = newState }

        ToggleImperial newState ->
            { options | imperialMeasure = newState }

        SetCurtainStyle newStyle ->
            { options | curtainStyle = newStyle }

        SetVerticalExaggeration value ->
            { options | verticalExaggeration = value }

        Terrain on ->
            { options | terrainOn = on }

        TerrainFineness fine ->
            { options | terrainFineness = fine }


encodeCurtain c =
    case c of
        NoCurtain ->
            0

        PlainCurtain ->
            1

        RainbowCurtain ->
            2

        PastelCurtain ->
            3


encodeOptions : DisplayOptions -> E.Value
encodeOptions options =
    E.object
        [ ( "roadPillars", E.bool options.roadPillars )
        , ( "roadCones", E.bool options.roadCones )
        , ( "roadTrack", E.bool options.roadTrack )
        , ( "problems", E.bool options.problems )
        , ( "centreLine", E.bool options.centreLine )
        , ( "terrainOn", E.bool options.terrainOn )
        , ( "seaLevel", E.bool options.seaLevel )
        , ( "withLighting", E.bool options.withLighting )
        , ( "terrainFineness", E.int options.terrainFineness )
        , ( "imperialMeasure", E.bool options.imperialMeasure )
        , ( "verticalExaggeration", E.float options.verticalExaggeration )
        , ( "curtainStyle", E.int (encodeCurtain options.curtainStyle) )
        ]


decodeOptions : E.Value -> DisplayOptions
decodeOptions json =
    let
        decoded =
            decodeValue temporaryDecoder json
    in
    case decoded of
        Ok restore ->
            { defaultDisplayOptions
                | roadPillars = restore.roadPillars
                , roadCones = restore.roadCones
                , roadTrack = restore.roadTrack
                , problems = restore.problems
                , centreLine = restore.centreLine
                , terrainOn = restore.terrainOn
                , seaLevel = restore.seaLevel
                , withLighting = restore.withLighting

                --, terrainFineness = restore.terrainFineness
                , imperialMeasure = restore.imperialMeasure
                , verticalExaggeration = restore.verticalExaggeration
                , curtainStyle =
                    case restore.curtainStyle of
                        1 ->
                            PlainCurtain

                        2 ->
                            RainbowCurtain

                        3 ->
                            PastelCurtain

                        _ ->
                            NoCurtain
            }

        _ ->
            defaultDisplayOptions


type alias DecodeTemporary =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , problems : Bool
    , centreLine : Bool
    , terrainOn : Bool
    , seaLevel : Bool
    , withLighting : Bool
    , terrainFineness : Int
    , imperialMeasure : Bool
    , verticalExaggeration : Float
    , curtainStyle : Int
    }


temporaryDecoder : Decoder DecodeTemporary
temporaryDecoder =
    Decode.succeed DecodeTemporary
        |> optional "roadPillars" bool defaultDisplayOptions.roadPillars
        |> optional "roadCones" bool defaultDisplayOptions.roadCones
        |> optional "roadTrack" bool defaultDisplayOptions.roadTrack
        |> optional "problems" bool defaultDisplayOptions.problems
        |> optional "centreLine" bool defaultDisplayOptions.centreLine
        |> optional "terrainOn" bool defaultDisplayOptions.terrainOn
        |> optional "seaLevel" bool defaultDisplayOptions.seaLevel
        |> optional "withLighting" bool defaultDisplayOptions.withLighting
        |> optional "terrainFineness" int defaultDisplayOptions.terrainFineness
        |> optional "imperialMeasure" bool defaultDisplayOptions.imperialMeasure
        |> optional "verticalExaggeration" float defaultDisplayOptions.verticalExaggeration
        |> optional "curtainStyle" int (encodeCurtain defaultDisplayOptions.curtainStyle)
