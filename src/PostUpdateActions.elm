module PostUpdateActions exposing (..)

import DisplayOptions exposing (DisplayOptions)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)


type
    PostUpdateAction
    -- This experimental pattern for returning information back to
    -- main about what needs to follow, since we can't know about the
    -- program at large, only our small part.
    = ActionTrackLoaded Track
    | ActionTrackChanged TrackEditType Track String
    | ActionPreview String (List TrackPoint)
    | ActionPointerMove TrackPoint
    | ActionFocusMove TrackPoint
    | ActionOptionsChanged DisplayOptions
    | ActionRepaintMap
    | ActionNoOp


type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
