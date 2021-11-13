module TrackEditType exposing (..)

type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
    | EditExtendsBeyondMarkers (Maybe (Int, Int))
    | EditNoOp -- only for Undo/Redo use
