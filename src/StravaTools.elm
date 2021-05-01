module StravaTools exposing (..)

import Element exposing (..)
import Element.Input as Input exposing (button)
import OAuthTypes as O exposing (Flow(..))
import StravaAuth exposing (getStravaToken)
import StravaTypes exposing (StravaRouteStatus(..), StravaSegmentStatus(..))
import Url exposing (Protocol(..), Url)
import ViewPureStyles exposing (prettyButtonStyles)


type Msg
    = UserChangedRouteId String
    | LoadExternalRoute


type alias Options =
    { externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    }


defaultOptions : Options
defaultOptions =
    { externalSegmentId = ""
    , externalRouteId = ""
    , externalSegment = SegmentNone
    , stravaRoute = StravaRouteNone
    }


stravaRouteOption : O.Model -> Options -> (Msg -> msg) -> Element msg
stravaRouteOption auth options wrap =
    let
        routeIdField =
            Input.text [ width (px 150) ]
                { onChange = wrap << UserChangedRouteId
                , text = options.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| text "Strava route ID"
                , label = Input.labelHidden "Strava route ID"
                }

        routeButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrap LoadExternalRoute
                , label = text <| "Fetch route"
                }
    in
    case getStravaToken auth of
        Just token ->
            row [ spacing 10 ]
                [ routeIdField
                , routeButton
                ]

        Nothing ->
            none
