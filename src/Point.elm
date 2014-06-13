module Point where

type Point a =
    { a
    | x : Float
    , y : Float
    }

point x y =
    { x = x
    , y = y
    }

distance p1 p2 =
    sqrt <|   (abs (p1.x - p2.x))^2.0
            + (abs (p1.y - p2.y))^2.0

mirror p = point p.y p.x
