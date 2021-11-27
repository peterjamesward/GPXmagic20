module PostUpdateActions exposing (..)

import GpxSource exposing (GpxSource)
import Track exposing (Track)
import TrackPoint exposing (TrackPoint)


type PostUpdateAction trck cmd
    = ActionTrackChanged TrackEditType (UndoEntry trck)
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
    | ActionFetchMapElevations


type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
    | EditExtendsBeyondMarkers (Maybe ( Int, Int ))
    | EditNoOp -- only for Undo/Redo use



-- This may not stay here.


type alias UndoEntry track =
    -- Use the old track points to revert, editFunction to redo.
    { label : String
    , firstChangedPoint : Int
    , lastChangedPoint : Int
    , oldTrackpoints : List TrackPoint
    , editFunction : { track | trackPoints : List TrackPoint} -> List TrackPoint
    }
