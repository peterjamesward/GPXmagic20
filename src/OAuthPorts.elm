port module OAuthPorts exposing (randomBytes, genRandomBytes)



port randomBytes : (List Int -> msg) -> Sub msg
port genRandomBytes : Int -> Cmd msg

