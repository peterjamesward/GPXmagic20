module Scene exposing (..)

import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type alias Scene =
    List (Entity LocalCoords)
