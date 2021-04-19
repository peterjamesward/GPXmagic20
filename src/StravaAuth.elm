module StravaAuth exposing (..)

import Base64.Encode as Base64
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (after)
import Element exposing (..)
import Element.Input exposing (button)
import Http
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import OAuthPorts exposing (genRandomBytes)
import OAuthTypes exposing (..)
import StravaClientSecret
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder


{-| OAuth configuration.

Note that this demo also fetches basic user information with the obtained access token,
hence the user info endpoint and JSON decoder

<http://www.strava.com/oauth/authorize?client_id=59195&response_type=code&redirect_uri=http://localhost/exchange_token&scope=read>

<https://www.strava.com/api/v3/athlete>

-}
configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "www.strava.com", path = "/oauth/authorize" }
    , tokenEndpoint =
        { defaultHttpsUrl | host = "www.strava.com", path = "/oauth/token" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "www.strava.com", path = "/api/v3/athlete" }
    , userInfoDecoder =
        Json.map3 UserInfo
            (Json.field "id" Json.int)
            (Json.field "firstname" Json.string)
            (Json.field "lastname" Json.string)
    , clientId =
        "59195"
    , clientSecret = StravaClientSecret.clientSecret
    , scope =
        [ "read_all" ]
    }


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}
init : Maybe { state : String } -> Url -> Key -> (OAuthMsg -> msgType) -> ( Model, Cmd msgType )
init mflags origin navigationKey wrapperMsg =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case OAuth.parseCode origin of
        OAuth.Empty ->
            ( { flow = Idle, redirectUri = redirectUri }
            , Cmd.none
            )

        -- It is important to set a `state` when making the authorization request
        -- and to verify it after the redirection. The state can be anything but its primary
        -- usage is to prevent cross-site request forgery; at minima, it should be a short,
        -- non-guessable string, generated on the fly.
        --
        -- We remember any previously generated state  state using the browser's local storage
        -- and give it back (if present) to the elm application upon start
        OAuth.Success { code, state } ->
            case mflags of
                Nothing ->
                    ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state then
                        ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { flow = Authorized code, redirectUri = redirectUri }
                        , clearUrl
                        )

        OAuth.Error error ->
            ( { flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }
            , clearUrl
            )


getUserInfo : Configuration -> OAuth.Token -> Cmd OAuthMsg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


update : OAuthMsg -> Model -> ( Model, Cmd OAuthMsg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized code, AccessTokenRequested ) ->
            accessTokenRequested model code

        ( Authorized _, GotAccessToken authenticationResponse ) ->
            gotAccessToken model authenticationResponse

        ( Authenticated token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authenticated _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Done _ _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            noOp model


noOp : Model -> ( Model, Cmd OAuthMsg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd OAuthMsg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd OAuthMsg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd OAuthMsg )
userInfoRequested model token =
    ( { model | flow = Authenticated token }
    , getUserInfo configuration token
    )


getAccessToken : Configuration -> Url -> OAuth.AuthorizationCode -> Cmd OAuthMsg
getAccessToken { clientId, tokenEndpoint } redirectUri code =
    Http.request <|
        OAuth.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId
                , secret = Just StravaClientSecret.clientSecret
                }
            , code = code
            , url = tokenEndpoint
            , redirectUri = redirectUri
            }


accessTokenRequested : Model -> OAuth.AuthorizationCode -> ( Model, Cmd OAuthMsg )
accessTokenRequested model code =
    ( { model | flow = Authorized code }
    , getAccessToken configuration model.redirectUri code
    )


gotAccessToken : Model -> Result Http.Error OAuth.AuthenticationSuccess -> ( Model, Cmd OAuthMsg )
gotAccessToken model authenticationResponse =
    case authenticationResponse of
        Err (Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    ( { model | flow = Errored <| ErrAuthentication error }
                    , Cmd.none
                    )

                _ ->
                    ( { model | flow = Errored ErrHTTPGetAccessToken }
                    , Cmd.none
                    )

        Err _ ->
            ( { model | flow = Errored ErrHTTPGetAccessToken }
            , Cmd.none
            )

        Ok { token } ->
            ( { model | flow = Authenticated token }
            ,  Cmd.none
            )


gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd OAuthMsg )
gotUserInfo model userInfoResponse =
    case ( model.flow, userInfoResponse ) of
        ( _, Err _ ) ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        ( Authenticated token, Ok userInfo ) ->
            ( { model | flow = Done userInfo token }
            , Cmd.none
            )

        _ ->
            ( { model | flow = Errored ErrStateMismatch }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd OAuthMsg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


stravaButton : Model -> (OAuthMsg -> msg) -> Element msg
stravaButton model msgWrapper =
    --TODO: Strava logo.
    let
        styles =
            []

        imgUrl =
            Builder.relative [ "images", "btn_strava_connectwith_orange.svg" ] []
    in
    case model.flow of
        Done userInfo _ ->
            column []
                [ text "Connected to Strava as"
                , text <| userInfo.firstname ++ " " ++ userInfo.lastname
                ]

        _ ->
            button
                styles
                { onPress = Just <| msgWrapper SignInRequested
                , label =
                    image
                        [ mouseOver [ alpha 0.7 ]
                        ]
                        { src = imgUrl
                        , description = "Connect to Strava"
                        }
                }


getStravaToken : Model -> Maybe OAuth.Token
getStravaToken model =
    case model.flow of
        Done info token ->
            Just token

        Authenticated token ->
            Just token

        _ ->
            Nothing


getStravaAthlete : Model -> Maybe Int
getStravaAthlete model =
    case model.flow of
        Done info token ->
            Just info.id

        _ ->
            Nothing
