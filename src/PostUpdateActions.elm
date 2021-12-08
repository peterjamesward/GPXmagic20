module PostUpdateActions exposing (..)

import GpxSource exposing (GpxSource)
import Graph exposing (Graph)
import Track exposing (Track)
import TrackEditType exposing (TrackEditType)
import TrackPoint exposing (TrackPoint)


type PostUpdateAction trck cmd
    = ActionTrackChanged TrackEditType UndoEntry
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



type alias EditResult =
    { before : List TrackPoint
    , edited : List TrackPoint
    , after : List TrackPoint
    , earthReferenceCoordinates : ( Float, Float, Float )
    , graph : Maybe Graph
    }


type alias EditFunction =
    Track -> EditResult


defaultEditResult : EditResult
defaultEditResult =
    { before = []
    , edited = []
    , after = []
    , earthReferenceCoordinates = ( 0.0, 0.0, 0.0 )
    , graph = Nothing
    }


type alias UndoEntry =
    -- Use the editFunction to do or redo, other to undo.
    -- These can use 'always' if the function is not (readily) reversible.
    -- These functions should return only the changed "middle section" of track.
    -- Or (thinking) they could return three parts. That way, we can just use the middle
    -- part for the preview but easily concat them for the whole track.
    -- Seems that would avoid more work elsewhere since we need to split it anyway.
    { label : String
    , editFunction : EditFunction
    , undoFunction : EditFunction
    , newOrange : Int
    , newPurple : Maybe Int
    , oldOrange : Int
    , oldPurple : Maybe Int
    }


defaultUndoEntry : UndoEntry
defaultUndoEntry =
    { label = "You should not see this text"
    , editFunction = always defaultEditResult
    , undoFunction = always defaultEditResult
    , newOrange = 0
    , newPurple = Nothing
    , oldOrange = 0
    , oldPurple = Nothing
    }


editAsTrack : EditFunction -> Track -> Track
editAsTrack f t =
    let
        result =
            f t
    in
    { t
        | trackPoints =
            TrackPoint.prepareTrackPoints <|
                result.before
                    ++ result.edited
                    ++ result.after
    }


editAsPreview : EditFunction -> Track -> List TrackPoint
editAsPreview f t =
    let
        result =
            f t
    in
    result.edited
