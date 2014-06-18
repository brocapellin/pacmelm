module Treasure where

import LineSegment
import LineSegment (lineSegment)

import Point
import Point (point)

import Axis

type State = [Treasure]

data Type = Normal | Cherry
type Treasure =
    { position : Point.Point
    , kind : Type
    }

treasure : Type -> Point.Point -> Treasure
treasure kind position =
    { position = position
    , kind = kind
    }

points : Treasure -> Int
points t = case t.kind of
  Normal -> 10
  Cherry -> 100

initialState : State
initialState = concatMap (generateTreasure Normal 0.5)
               treasureSegments

generateTreasure
  : Type
 -> Float
 -> LineSegment.LineSegment
 -> [Treasure]
generateTreasure kind unit lineSegment =
  let
    points = LineSegment.sample unit lineSegment
  in
    map (treasure kind) points

treasureSegments : [LineSegment.LineSegment]
treasureSegments =
    [ lineSegment (point -5.0 5.0) 10.0 Axis.X
    , lineSegment (point -5.0 -5.0) 10.0 Axis.Y 
    , lineSegment (point 5.0 -5.0) 10.0 Axis.Y
    , lineSegment (point -10.0 0.0) 5.0 Axis.X
    , lineSegment (point 5.0 0.0) 5.0 Axis.X 
    ]
