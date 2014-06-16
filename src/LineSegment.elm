module LineSegment where

import Point
import Axis

type LineSegment =
    { start  : Point.Point
    , length : Float
    , axis   : Axis.Axis
    }

lineSegment
  : Point.Point
 -> Float
 -> Axis.Axis
 -> LineSegment
lineSegment start length axis =
    { start  = start
    , length = length
    , axis   = axis
    }

mirror : LineSegment -> LineSegment
mirror lineSegment =
    { lineSegment
    | start <- Point.mirror lineSegment.start
    , axis  <- Axis.mirror lineSegment.axis
    } 

distancePoint
  : Point.Point
 -> LineSegment
 -> Float
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
    Axis.Y    -> distancePoint
                    (Point.mirror point)
                    (mirror lineSegment)
    otherwise -> sqrt (distx^2.0 + disty^2.0)

constrain
  : Point.Point
 -> LineSegment
 -> Point.Point
constrain point lineSegment =
  let
    p = point
    s = lineSegment
  in case lineSegment.axis of
    Axis.Y    -> Point.mirror
                    <| constrain
                           (Point.mirror p)
                           (mirror s) 
    otherwise -> Point.point
                    ( (max s.start.x . min p.x)
                      (s.start.x + s.length)
                    )
                    s.start.y

sample : Float -> LineSegment -> [Point.Point]
sample unit lineSegment =
  let
    axis = lineSegment.axis

    start = case axis of
      Axis.X    -> lineSegment.start.x
      otherwise -> lineSegment.start.y

    partialPoints =
        range unit start (start + lineSegment.length)

    toPoint partialPoint =
        case axis of
          Axis.Y    -> Point.point
            lineSegment.start.x partialPoint
          otherwise -> Point.point
            partialPoint lineSegment.start.y

  in
    map toPoint partialPoints 
     
range : Float -> Float -> Float -> [Float]
range step start end =
  if start > end
  then []
  else start :: range step (start + step) end
