module Geometry101 exposing (..)

import Arc2d
import Length
import List
import Point2d exposing (xCoordinate, yCoordinate)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Road =
    { startAt : Point
    , endsAt : Point
    }


type alias Circle =
    { centre : Point
    , radius : Float
    }


type alias LineEquation =
    { a : Float
    , b : Float
    , c : Float
    }


type alias Matrix =
    { tl : Float
    , tr : Float
    , bl : Float
    , br : Float
    }


type alias Column =
    { t : Float
    , b : Float
    }


type alias Row =
    { l : Float
    , r : Float
    }


det v1 v2 =
    v1.x * v2.x + v1.y * v2.y


dot v1 v2 =
    v1.x * v2.y - v1.y * v2.x


angle v1 v2 =
    atan2 (det v1 v2) (dot v1 v2)


distance p1 p2 =
    sqrt <| (p1.x - p2.x) ^ 2.0 + (p1.y - p2.y) ^ 2.0


pointsToGeometry : Point -> Point -> Road
pointsToGeometry p1 p2 =
    { startAt = p1, endsAt = p2 }


isBefore : Road -> Point -> Bool
isBefore r p =
    antiInterpolate p r.startAt r.endsAt < 0.0


isAfter : Road -> Point -> Bool
isAfter r p =
    antiInterpolate p r.startAt r.endsAt > 1.0


pointAlongRoad : Road -> Float -> Point
pointAlongRoad road distanceFromStart =
    let
        roadLength =
            distance road.startAt road.endsAt
    in
    interpolateLine
        (distanceFromStart / roadLength)
        road.startAt
        road.endsAt


interpolateScalar fraction a b =
    b * fraction + a * (1.0 - fraction)


interpolateLine fraction p1 p2 =
    -- Find p3 on p1-p2, such that |p1p3|/|p1p2| = fraction
    -- Expecting fraction to be in [0,1] but doesn't have to be.
    { x = interpolateScalar fraction p1.x p2.x
    , y = interpolateScalar fraction p1.y p2.y
    }


antiInterpolate : Point -> Point -> Point -> Float
antiInterpolate p pa pb =
    -- Supports testing for converging roads.
    -- Express p as fraction along AB so at A is 0, at B is 1.
    -- Assumes points are co-linear, meaningless otherwise.
    let
        aDist =
            distance p pa

        bDist =
            distance p pb

        ab =
            distance pa pb
    in
    if aDist + bDist <= ab then
        -- Interior point
        aDist / ab

    else if aDist > bDist then
        -- It's outboard on the B side
        aDist / ab

    else if bDist > aDist then
        -1.0 * (aDist / ab)

    else
        -- No idea where it is
        0.0



{-
   This is about helping to smooth a bend.
   In one case, maybe are looking at only one vertex.
   That is to say, we have one road segment B-P and the next P-C.
   Then we have the three points we need to find the incenter and incircle.
   Note that we are implicitly.

   More generally, suppose we have segments
   AB, BC, CD, DE, EF, FG
   We select at least two contiguous segments.
   Suppose we select (BC, CD).
   This means that points B and D remain, point C will be replaced with
   new segments derived from the incircle of BCD.
   We don't need to find the intersection of BC and CD, we know it.

   Suppose we select (BC, CD, DE).
   This means that B and E remain, C and D will be replaced with
   new segments derived from the incircle of BCP,
   where P is the intersect of (the extensions of) BC and DE.
-}


findIncircleFromRoads : List Road -> Maybe Circle
findIncircleFromRoads roads =
    case roads of
        [] ->
            Nothing

        [ _ ] ->
            Nothing

        [ r1, r2 ] ->
            findIncircleFromTwoRoads r1 r2

        r1 :: rs ->
            findIncircleFromRoads <| r1 :: List.take 1 (List.reverse rs)


findIncircleFromTwoRoads : Road -> Road -> Maybe Circle
findIncircleFromTwoRoads r1 r2 =
    let
        intersection =
            if r1.endsAt == r2.startAt then
                Just r1.endsAt

            else
                findIntercept r1 r2
    in
    case intersection of
        Just p ->
            Just <| findIncircle r1.startAt r2.endsAt p

        Nothing ->
            Nothing


findIncircle : Point -> Point -> Point -> Circle
findIncircle pA pB pC =
    {-
       The centre of the inscribed triangle (incentre) requires the lengths of the sides.
       (The naming convention is that side b is opposite angle B, etc.)
       |b| = |CP| = 2.0
       |c| = |BP| = 1.0
       |p| = |BC| = sqrt 5 = 2.236

       X = (|b|.Bx + |c|.Cx + |p|.Px) / (|b| + |c| + |p|)
         = (2.0 * 3 + 1.0 * 4 + 2.236 * 4) / 5.236
         = 3.618

       Y = (|b|.By + |c|.Cy + |p|.Py) / (|b| + |c| + |p|)
         = (2.0 * 5 + 1.0 * 3 + 2.236 * 5) / 5.236
         = 4.618

       We also derive the radius of the incircle:

       r = sqrt <| (s - b)(s - c)(s - p)/s, where s = (b + c + p)/2

       That should give us enough information to determine the tangent points.
       (The triangle formed by these touchpoints is the Gergonne triangle.)

       In our case, s = 2.618
       r = sqrt <| (2.618 - 2)(2.618 - 1)(2.618 - 2.236)/2.618
         = sqrt 0.1459
         = 0.382
    -}
    let
        ( a, b, c ) =
            ( distance pB pC, distance pA pC, distance pA pB )

        perimeter =
            a + b + c

        semi =
            perimeter / 2.0

        r =
            sqrt <| (semi - a) * (semi - b) * (semi - c) / semi

        x =
            (a * pA.x + b * pB.x + c * pC.x) / perimeter

        y =
            (a * pA.y + b * pB.y + c * pC.y) / perimeter
    in
    { centre = { x = x, y = y }
    , radius = r
    }


findIntercept : Road -> Road -> Maybe Point
findIntercept r1 r2 =
    {-
       The intercept P of AB and CD, if it exists, satisfies both equations.

           0 x + 2 y -10 == 0
       &&  2 x - 2 y -2  == 0

       In matrix form  | 0 2  | | x |    | -10 |
                       | 2 -2 | | y | == |  +2 |

       By inverting and multiplying through, the intersect P is
       | x | = | 4 |
       | y |   | 5 |

       We now have three points:
       B = (3,5)    C = (4,3)   P = (4,5)


       Now let us try to draw this circle on the third person view!
    -}
    let
        r1Line =
            lineEquationFromTwoPoints r1.startAt r1.endsAt

        r2Line =
            lineEquationFromTwoPoints r2.startAt r2.endsAt
    in
    lineIntersection r1Line r2Line


lineIntersection : LineEquation -> LineEquation -> Maybe Point
lineIntersection l1 l2 =
    let
        matrix =
            { tl = l1.a
            , tr = l1.b
            , bl = l2.a
            , br = l2.b
            }

        column =
            { t = -1.0 * l1.c, b = -1.0 * l2.c }

        inv =
            matrixInverse matrix
    in
    case inv of
        Just inverse ->
            let
                col =
                    matrixMultiplyColumn inverse column
            in
            Just { x = col.t, y = col.b }

        Nothing ->
            Nothing


solveQuadratic : Float -> Float -> Float -> List Float
solveQuadratic a b c =
    let
        disc =
            b * b - 4 * a * c
    in
    if disc == 0 then
        [ 0 - b / (a + a) ]

    else if disc > 0 then
        [ (0 - b - sqrt disc) / (a + a)
        , (0 - b + sqrt disc) / (a + a)
        ]

    else
        []


lineCircleIntersections : LineEquation -> Circle -> List Point
lineCircleIntersections { a, b, c } { centre, radius } =
    -- Line equation is Ax + By + C = 0.
    -- Circle is in { centre, radius } form
    let
        shiftedLine =
            -- Shift so that we can use centre of circle as origin pro tem.
            { a = a, b = b, c = c - (a * centre.x + b * centre.y) }

        xSolutionsShifted =
            -- We can solve a quadratic, to yield 0, 1, or 2 x values.
            solveQuadratic
                (shiftedLine.a * shiftedLine.a + shiftedLine.b * shiftedLine.b)
                (2.0 * shiftedLine.a * shiftedLine.c)
                (shiftedLine.c * shiftedLine.c - shiftedLine.b * shiftedLine.b * radius * radius)

        xSolutions =
            List.map ((+) centre.x) xSolutionsShifted

        ySolutions =
            List.map (\x -> 0 - (a * x + c) / b) xSolutions
    in
    List.map2 Point xSolutions ySolutions


matrixInverse : Matrix -> Maybe Matrix
matrixInverse m =
    let
        determinant =
            m.tl * m.br - m.tr * m.bl
    in
    if abs determinant < 10 ^ -20 then
        Nothing

    else
        Just
            { tl = m.br / determinant
            , tr = -1.0 * m.tr / determinant
            , bl = -1.0 * m.bl / determinant
            , br = m.tl / determinant
            }


matrixMultiplyColumn : Matrix -> Column -> Column
matrixMultiplyColumn m c =
    { t = m.tl * c.t + m.tr * c.b
    , b = m.bl * c.t + m.br * c.b
    }


lineEquationFromTwoPoints : Point -> Point -> LineEquation
lineEquationFromTwoPoints p1 p2 =
    {-
       An arrangement of the two point line equation is:
       (y1 - y2) X + (x2 - x1) Y + (x1.y2 - x2.y1) = 0

       For AB this is
       (5.0 - 5.0) X + (3.0 - 1.0) Y + (5.0 - 15.0) = 0
       Thus A = 0, B = 2, C = -10

       To check, for (1,5) : 0 x 1 + 2 x 5 + (-10) == 0
                 for (3,5) : 0 x 3 + 2 x 5 + (-10) == 0

       For CD:
       (3.0 - 1.0) X + (2.0 - 4.0) Y + (4.0 - 6.0) = 0
       Thus A = 2, B = -2, C = -2

       To check, for (4,3) : 2 x 4 + (-2) x 3 + (-2) == 0
                 for (2,1) : 2 x 2 + (-2) x 1 + (-2) == 0
    -}
    let
        a =
            p1.y - p2.y

        b =
            p2.x - p1.x

        c =
            p1.x * p2.y - p2.x * p1.y
    in
    { a = a, b = b, c = c }


linePerpendicularTo : LineEquation -> Point -> LineEquation
linePerpendicularTo line p =
    -- Perpendicular passing through point.
    let
        aybx =
            line.a * p.y - line.b * p.x
    in
    { a = line.b, b = -1.0 * line.a, c = aybx }
