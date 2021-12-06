module PostUpdateActions exposing (..)

import GpxSource exposing (GpxSource)
import Track exposing (Track)
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


type TrackEditType
    = EditPreservesIndex
    | EditPreservesNodePosition
    | EditNoOp -- only for Undo/Redo use


type alias EditFunction =
    Track -> ( List TrackPoint, List TrackPoint, List TrackPoint )


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
    , editFunction = always ( [], [], [] )
    , undoFunction = always ( [], [], [] )
    , newOrange = 0
    , newPurple = Nothing
    , oldOrange = 0
    , oldPurple = Nothing
    }


editAsTrack : EditFunction -> Track -> Track
editAsTrack f t =
    let
        ( a, b, c ) =
            f t
    in
    { t | trackPoints = TrackPoint.prepareTrackPoints <| a ++ b ++ c }


editAsPreview : EditFunction -> Track -> List TrackPoint
editAsPreview f t =
    let
        ( a, b, c ) =
            f t
    in
    b
