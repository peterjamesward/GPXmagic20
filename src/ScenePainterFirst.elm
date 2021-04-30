module ScenePainterFirst exposing (..)


firstPersonCamera : Model -> Maybe (Camera3d Length.Meters LocalCoords)
firstPersonCamera model =
    let
        eyePoint road =
            case model.flythrough of
                Nothing ->
                    Point3d.translateBy
                        (Vector3d.meters 0.0 0.0 eyeHeight)
                        road.startsAt.xyz

                Just flying ->
                    flying.cameraPosition

        cameraViewpoint road =
            case model.flythrough of
                Nothing ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint road
                        , focalPoint =
                            Point3d.translateBy
                                (Vector3d.meters 0.0 0.0 eyeHeight)
                                road.endsAt.xyz
                        , upDirection = Direction3d.positiveZ
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = eyePoint road
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }

        cappedNodeNumber =
            min model.currentNode (Array.length model.nodeArray - 2)
    in
    case Array.get cappedNodeNumber model.roadArray of
        Just road ->
            Just <|
                Camera3d.perspective
                    { viewpoint = cameraViewpoint road
                    , verticalFieldOfView = Angle.degrees <| 120.0 / (1 + model.zoomLevelFirstPerson / 2.0)
                    }

        Nothing ->
            Nothing


viewRoadSegment : Model -> Element Msg
viewRoadSegment model =
    case model.currentSceneCamera of
        Just camera ->
            row [ padding 5, spacing 10 ]
                [ el
                    withMouseCapture
                  <|
                    html <|
                        render3dScene model.displayOptions.withLighting
                            { camera = camera
                            , dimensions = view3dDimensions
                            , background = Scene3d.backgroundColor Color.lightBlue
                            , clipDepth = Length.meters 1.0
                            , entities =
                                model.varyingVisualEntities
                                    ++ model.staticVisualEntities
                                    ++ model.terrainEntities
                            }
                , zoomSlider model.zoomLevelFirstPerson ZoomLevelFirstPerson
                ]

        Nothing ->
            none
