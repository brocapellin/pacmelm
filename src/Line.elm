module Line where

segment x y length axis =
    { start  = { x = x
               , y = y
               }
    , length = length
    , axis   = axis
    }

mirrorPoint p = 
    { x = p.y
    , y = p.x
    }

mirrorAxis a =
  case a of
    X         -> Y
    otherwise -> X 

mirrorLineSegment ls =
    { ls
      | start <- mirrorPoint ls.start
      , axis  <- mirrorAxis ls.axis
    } 

distance point lineSegment =
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
    Y         -> distance
                    (mirrorPoint point)
                    (mirrorLineSegment lineSegment)
    otherwise -> sqrt (distx^2.0 + disty^2.0)

constrainToClosest point lineSegments =
  let
    withDist   = map (\l -> (distance point l,l))
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
    Y         -> mirrorPoint
                    <| constrain
                       (mirrorPoint p)
                       (mirrorLineSegment s) 
    otherwise -> { x = (max s.start.x . min p.x)
                        (s.start.x + s.length)
                 , y = s.start.y
                 }

data Axis = X | Y
