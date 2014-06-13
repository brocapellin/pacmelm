module LineSegment where

import Point

type LineSegment a b =
    { a
    | start  : Point.Point b
    , length : Float
    , axis   : Axis
    }

data Axis = X | Y

lineSegment start length axis =
    { start  = start
    , length = length
    , axis   = axis
    }

mirror lineSegment =
    { lineSegment
    | start <- Point.mirror lineSegment.start
    , axis  <- if lineSegment.axis == Y
               then X else Y
    } 

distancePoint point lineSegment =
  let
    p  = point
    s  = lineSegment
    se = s.start.x + s.length

    disty         = abs (p.y - s.start.y)
    distx         =
        if p.x >= s.start.x && p.x <= se
        then 0.0
        else min (abs (p.x - s.start.x)) (abs (p.x - se))

  in case lineSegment.axis of
    Y         -> distancePoint
                    (Point.mirror point)
                    (mirror lineSegment)
    otherwise -> sqrt (distx^2.0 + disty^2.0)

constrainToClosest point lineSegments =
  let
    withDist   = map (\l -> (distancePoint point l,l))
    sortByDist = sortBy fst
    target     = (snd . head . sortByDist . withDist)
                 lineSegments
  in
    if | isEmpty lineSegments -> point
       | otherwise            -> constrain point target

constrain point lineSegment =
  let
    p = point
    s = lineSegment
  in case lineSegment.axis of
    Y         -> Point.mirror
                    <| constrain
                           (Point.mirror p)
                           (mirror s) 
    otherwise -> Point.point
                    ( (max s.start.x . min p.x)
                      (s.start.x + s.length)
                    )
                    s.start.y
