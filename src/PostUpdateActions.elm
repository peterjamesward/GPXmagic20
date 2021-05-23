module PostUpdateActions exposing (..)

import OAuth.GpxSource exposing (GpxSource)
import Track exposing (Track)
import TrackEditType exposing (TrackEditType)
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
    | ActionWalkGraph
    | ActionToggleMapDragging Bool


