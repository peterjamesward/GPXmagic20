module PostUpdateActions exposing (..)

import Track exposing (Track)
import TrackPoint exposing (TrackPoint)


type
    PostUpdateAction
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


type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
