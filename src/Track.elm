module Track exposing (..)

import Graph exposing (Graph)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import TrackPoint exposing (TrackPoint)
import Vector3d exposing (Vector3d)


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    , transform : Vector3d Meters LocalCoords
    }

