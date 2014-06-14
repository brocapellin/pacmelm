module Point where

type Point =
    { x : Float
    , y : Float
    }

point : Float -> Float -> Point 
point x y =
    { x = x
    , y = y
    }

distance : Point -> Point -> Float
distance p1 p2 =
    sqrt <|   (abs (p1.x - p2.x))^2.0
            + (abs (p1.y - p2.y))^2.0

mirror : Point -> Point
mirror p = 
    { x = p.y
    , y = p.x
    }
