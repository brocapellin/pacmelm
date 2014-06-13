module Circle where

import Point

type Circle a b =
    { a
    | origin : Point.Point b
    , radius : Float
    }

circle origin radius =
    { origin = origin
    , radius = radius
    }

intersectsPoint circle point =
    Point.distance circle.origin point <= circle.radius
