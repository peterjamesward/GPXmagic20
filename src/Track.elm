module Track exposing (..)

import Graph exposing (Graph)
import TrackPoint exposing (TrackPoint)


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    }

