module ImagePostUpdateActions exposing (..)

import TrackPoint exposing (TrackPoint)


type
    PostUpdateAction
    -- This experimental pattern for returning information back to
    -- main about what needs to follow, since we can't know about the
    -- program at large, only our small part.
    = ImageOnly
    | PointerMove TrackPoint
    | ImageNoOp
