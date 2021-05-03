module PostUpdateActions exposing (..)

import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import OAuth.GpxSource exposing (GpxSource)
import Point3d exposing (Point3d)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)


type
    PostUpdateAction cmd
    -- This experimental pattern for returning information back to
    -- main about what needs to follow, since we can't know about the
    -- program at large, only our small part.
    = ActionTrackChanged TrackEditType Track String
    | ActionRerender
    | ActionPreview
    | ActionPointerMove TrackPoint
    | ActionMarkerMove (Maybe TrackPoint)
    | ActionFocusMove TrackPoint
    | ActionRepaintMap
    | ActionNoOp
    | ActionCommand cmd
    | ActionNewRoute String GpxSource


type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
