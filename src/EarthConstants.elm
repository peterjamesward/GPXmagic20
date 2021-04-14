module EarthConstants exposing (..)


metresPerDegree =
    -- a degree of longitude at the equator ...
    metresPerPixelAtEquatorZoomZero


metresPerPixelAtEquatorZoomZero =
    78271.484


metresPerPixel zoomLevel latitude =
    cos latitude * metresPerPixelAtEquatorZoomZero / 2.0 ^ zoomLevel
