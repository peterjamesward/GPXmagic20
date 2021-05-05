module TrackEditType exposing (..)

type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
    | EditNoOp -- only for Undo/Redo use
