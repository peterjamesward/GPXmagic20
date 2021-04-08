module Rendering exposing (..)

import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type alias Rendering =
    List (Entity LocalCoords)
