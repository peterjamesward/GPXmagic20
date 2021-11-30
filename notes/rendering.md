Previously, I wrote about the effect of switching from simple list search to 
using a two-dimensional index (a type of quadtree), and the impact of moving
code from the application at large into the data structure, reducing the overheads
of creating results sets only for them to be filtered or reduced in some way but
essentially discarded.

The widespread use of lists is part and parcel of GPXmagic however. It's an editor 
and visualiser for lists of GPS coordinates; that's what a GPX file _is_ Being 
many-times bitten by premature optimisation, I am generally fairly careful to avoid any.
for example, as you can see in the screenshot above, there's a few options for how
to visualise a track.

Up until now, I have not tried to optimise this. I do most of my functional testing
on small to moderate tracks, with perhaps a few thousand GPS points. Here's a snippet of code
that I use to render.

```elm
    ...     
        scene =
            List.concat
                [ graphNodes
                , if options.centreLine then
                    mapOverPairs (centreLineBetween gradientColourPastel)

                  else
                    []
                , if options.roadTrack then
                    mapOverPairs paintSurfaceBetween

                  else
                    []
                , if options.curtainStyle /= NoCurtain && not options.terrainOn then
                    mapOverPairs (curtainBetween floorPlane gradientFunction)

                  else
                    []
                , if options.roadPillars && not options.terrainOn then
                    mapOverPoints (roadSupportPillar floorPlane)

                  else
                    []
                , if options.roadCones && not options.terrainOn then
                    mapOverPoints trackPointCone

                  else
                    []
                ]
    in
    scene

```

It makes a pass across the list for each of the five possible elements, each produces a list
of WebGL visual elements, which are concatenated at the end. 
It's fairly clear. It runs reasonably quickly. Is there a problem?

Imagine this running over one of my larger examples. It has 23,000 points. At worst, we're 
traversing this five times. Each traversal constructs an even longer list of visual elements.
We may then have five lists of (say) 50,000 elements each. These are then concatenated into
a final result of 250,000 elements. The partial lists are then discarded for the garbage collector
to recycle. Our peak memory usage is far higher than it need be.

It's actually worse, since `mapOverPairs` casually adds to the overhead:

```elm
        mapOverPairs f =
            List.concat <| List.map2 f track (List.drop 1 track)
```

To get to the point, I'm trying to reduce memory usage in general, and it's obvious from the browser
developer console that adding graphic elements consumes tens of megabytes, so this seems like something
worth trying.

All we need do is restructure the control flow so that we make one pass over the track and 
apply all the active options for each point, and eliminate the `mapOverPairs` while we're at it. 
Yet, I don't want to just move the `if ...` structure inside my traversal; that seems silly given
that they can't possibly change during the operation.

I ended up with this:

```elm
        scenePainterFunctions : List (TrackPoint -> Scene)
        scenePainterFunctions =
            [ if options.centreLine then
                Just (centreLineBetween gradientColourPastel)

              else
                Nothing
            , if options.roadTrack then
                Just paintSurfaceBetween

              else
                Nothing
            , if options.curtainStyle /= NoCurtain && not options.terrainOn then
                Just (curtainBetween floorPlane gradientFunction)

              else
                Nothing
            , if options.roadPillars && not options.terrainOn then
                Just (roadSupportPillar floorPlane)

              else
                Nothing
            , if options.roadCones && not options.terrainOn then
                Just trackPointCone

              else
                Nothing
            ]
                |> List.filterMap identity

        paintScenePart : TrackPoint -> Scene -> Scene
        paintScenePart pt accum =
            -- This is intended to allow us to touch each TP once, create all the
            -- active scene elements, and build a scene list with minimal overhead.
            scenePainterFunctions
                |> List.map (\fn -> fn pt)
                |> combineLists
                |> reversingCons accum

        scene : Scene
        scene =
            List.foldl paintScenePart [] track.trackPoints
```

Three blocks to explain. 

Firstly, `scenePainterFunctions` is the original `if ...` statement
modified to return either `Just <function>` or `Nothing` according to the options set, which
is then filtered to leave a list of the functions that will paint the selected features.

Secondly, `paintScenePart` is the function we use to make a single traversal of the points.
_For each point_, it apply each of the selected rendering functions, each of which returns
a small list of visual elements. These are combined into a single list for each point and
pushed onto the start of our accumulating list.

Thirdly, `scene` merely applies the `paintScenePart` operation to each point.

The helper functions `combineLists` and `reversingCons` simply put lists together 
efficiently, when the order of the elements is not important.

```elm
reversingCons : List a -> List a -> List a
reversingCons xs ys =
    -- Use this for speed when order can be ignored.
    case ( xs, ys ) of
        ( [], _ ) ->
            ys

        ( _, [] ) ->
            xs

        ( x :: moreX, _ ) ->
            reversingCons moreX (x :: ys)


combineLists : List (List a) -> List a
combineLists lists =
    List.foldl reversingCons [] lists
```

Big question: does it make any difference? To be honest here, I' struggling to figure this out.
The browser developer tools not unreasonably assume you're a JavaScript developer. I'm not. I'm
an Elm developer, and Elm compiles to dense, obscure JavaScript. Hence, the rather smart tools
for looking at memory allocation are hard to interpret. The outcome is "I think so", though
there's an element there of "I so want it to be so". Nonetheless, I like the shape of the code
and I shall sleep more soundly.

