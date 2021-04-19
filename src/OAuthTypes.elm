module OAuthTypes exposing (..)

import Http
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Url)


type alias Model =
    { redirectUri : Url
    , flow : Flow
    }


type Flow
    = Idle
    | Authorized OAuth.AuthorizationCode
    | Authenticated OAuth.Token
    | Done UserInfo OAuth.Token
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetUserInfo


type alias UserInfo =
    { id : Int
    , firstname : String
    , lastname : String
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , tokenEndpoint : Url
    , userInfoDecoder : Json.Decoder UserInfo
    , clientId : String
    , scope : List String
    , clientSecret : String
    }


type OAuthMsg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | AccessTokenRequested
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignOutRequested
