module GradientSmoother exposing (..)

import Angle
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Quantity
import SketchPlane3d
import Track exposing (Track)
import Utils exposing (showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


info =
    """## Gradient smoother

This is the 'classic' gradient-only smoother from v1.

At one extreme, it will provide a uniform gradient between
the two marker cones. At the other extreme, it will leave
all gradients unchanged. In-between, it will bring all gradients
closer to the uniform value, to some extent.

It will not affect spacing of points or bends. It can be 
useful just for taking the edge off of a rough stretch but
lacks any sophistication."""


type Msg
    = SmoothGradient Float
    | SetBumpinessFactor Float


type alias Options =
    { bumpinessFactor : Float
    }


defaultOptions =
    { bumpinessFactor = 0.5 }


viewGradientFixerPane : Options -> (Msg -> msg) -> Track -> Element msg
viewGradientFixerPane options wrapper track =
    let
        smoothnessSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetBumpinessFactor
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Bumpiness = "
                                ++ showDecimal2 options.bumpinessFactor
                , min = 0.0
                , max = 1.0
                , step = Nothing
                , value = options.bumpinessFactor
                , thumb = Input.defaultThumb
                }

        markedNode =
            Maybe.withDefault track.currentNode track.markedNode

        startPoint =
            if track.currentNode.index <= markedNode.index then
                track.currentNode

            else
                markedNode

        endPoint =
            if track.currentNode.index < markedNode.index then
                markedNode

            else
                track.currentNode

        avg =
            --TODO: This should be in Utils.
            Direction3d.from startPoint.xyz endPoint.xyz
                |> Maybe.map (Direction3d.elevationFrom SketchPlane3d.xy)
                |> Maybe.withDefault Quantity.zero
                |> Angle.tan
                |> (*) 100.0

        gradientSmoothControls =
            row [ spacing 5, padding 5 ]
                [ button
                    prettyButtonStyles
                    { onPress = Just <| wrapper <| SmoothGradient avg
                    , label =
                        text <|
                            "Smooth between markers\nAverage gradient "
                                ++ showDecimal2 avg
                    }
                , smoothnessSlider
                ]
    in
    column [ padding 10, spacing 10, centerX ] <|
        [ gradientSmoothControls ]
