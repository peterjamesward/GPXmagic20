module DisplayOptions exposing (..)

import Element exposing (..)
import Element.Input as Input
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, radioButton)


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
    | ToggleLighting Bool
    | ToggleSeaLevel Bool
    | ToggleRoad Bool
    | ToggleCones Bool
    | ToggleCentreLine Bool
    | SetCurtainStyle CurtainStyle
    | SetVerticalExaggeration Float


type Action
    = NoOp
    | ProfileChange Float


type alias DisplayOptions =
    { roadPillars : Bool
    , roadCones : Bool
    , roadTrack : Bool
    , curtainStyle : CurtainStyle
    , problems : Bool
    , centreLine : Bool
    , terrain : Bool
    , seaLevel : Bool
    , withLighting : Bool
    , verticalExaggeration : Float
    }


defaultDisplayOptions : DisplayOptions
defaultDisplayOptions =
    { roadPillars = True
    , roadCones = True
    , roadTrack = True
    , curtainStyle = PastelCurtain
    , problems = False
    , centreLine = False
    , terrain = False
    , seaLevel = True
    , withLighting = True
    , verticalExaggeration = 1.0
    }


viewDisplayOptions : DisplayOptions -> (Msg -> msg) -> Element msg
viewDisplayOptions options wrap =
    column [ width fill, centerX ]
        [ row [ spacing 5, padding 10 ]
            [ column [ spacing 5 ]
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
                ]
            , column [ spacing 5 ]
                [ Input.checkbox []
                    { onChange = wrap << TogglePillars
                    , icon = checkboxIcon
                    , checked = options.roadPillars
                    , label = Input.labelRight [ centerY ] (text "Road support pillars")
                    }
                , Input.checkbox []
                    { onChange = wrap << ToggleCones
                    , icon = checkboxIcon
                    , checked = options.roadCones
                    , label = Input.labelRight [ centerY ] (text "Trackpoint cones")
                    }
                , Input.checkbox []
                    { onChange = wrap << ToggleLighting
                    , icon = checkboxIcon
                    , checked = options.withLighting
                    , label = Input.labelRight [ centerY ] (text "Lighting")
                    }
                ]
            ]
        , row [ padding 5, centerX ]
            [ Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrap << SetVerticalExaggeration
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Elevation exaggeration = "
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


update : DisplayOptions -> Msg -> (Msg -> msg) -> ( DisplayOptions, Action )
update options dispMsg wrap =
    case dispMsg of
        TogglePillars newState ->
            ( { options | roadPillars = newState }
            , NoOp
            )

        ToggleLighting newState ->
            ( { options | withLighting = newState }
            , NoOp
            )

        ToggleSeaLevel newState ->
            ( { options | seaLevel = newState }
            , NoOp
            )

        ToggleRoad newState ->
            ( { options | roadTrack = newState }
            , NoOp
            )

        ToggleCones newState ->
            ( { options | roadCones = newState }
            , NoOp
            )

        ToggleCentreLine newState ->
            ( { options | centreLine = newState }
            , NoOp
            )

        SetCurtainStyle newStyle ->
            ( { options | curtainStyle = newStyle }
            , NoOp
            )

        SetVerticalExaggeration value ->
            ( { options | verticalExaggeration = value }
            , ProfileChange value
            )