module StravaTypes exposing (..)


type alias StravaSegment =
    { name : String
    , distance : Float
    , elevation_high : Float
    , elevation_low : Float
    , start_latitude : Float
    , start_longitude : Float
    , end_latitude : Float
    , end_longitude : Float
    }


type alias StravaRoute =
    { name : String
    , description : String
    , distance : Float
    , elevation_gain : Float
    }


type alias StravaDistanceStream =
    { is_type : String
    , data : List Float
    , series_type : String
    , original_size : Int
    , resolution : String
    }


type alias StravaAltitudeStream =
    { is_type : String
    , data : List Float
    , series_type : String
    , original_size : Int
    , resolution : String
    }


type alias StravaLatLng =
    { lat : Float
    , lng : Float
    }


type alias StravaLatLngStream =
    { is_type : String
    , data : List StravaLatLng
    , series_type : String
    , original_size : Int
    , resolution : String
    }


type alias StravaSegmentStreams =
    { latLngs : StravaLatLngStream
    , distances : StravaDistanceStream
    , altitude : StravaAltitudeStream
    }


type StravaSegmentStatus
    = SegmentNone
    | SegmentRequested
    | SegmentError String
    | SegmentOk StravaSegment
    | SegmentPreviewed StravaSegment
    | SegmentNotInRoute StravaSegment


type StravaRouteStatus
    = StravaRouteNone
    | StravaRouteRequested
    | StravaRouteOk StravaRoute
    | StravaRouteError String
