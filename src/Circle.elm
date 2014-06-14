module Circle where

import Point

type Circle =
    { origin : Point.Point
    , radius : Float
    }

circle : Point.Point -> Float -> Circle
circle origin radius =
    { origin = origin
    , radius = radius
    }

intersectsPoint : Circle -> Point.Point -> Bool
intersectsPoint circle point =
    Point.distance circle.origin point <= circle.radius
