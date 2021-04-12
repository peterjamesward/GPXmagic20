module Track exposing (..)

import Axis3d exposing (Axis3d)
import Graph exposing (Graph)
import Length exposing (Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (distanceFromAxis)
import TrackPoint exposing (TrackPoint)


type alias Track =
    { track : List TrackPoint
    , trackName : Maybe String
    , currentNode : TrackPoint
    , markedNode : Maybe TrackPoint
    , graph : Maybe Graph
    }


trackPointNearestRay : Track -> Axis3d Meters LocalCoords -> Maybe TrackPoint
trackPointNearestRay track ray =
    track.track
        |> List.Extra.minimumBy (Length.inMeters << distanceFromAxis ray << .xyz)
