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

initialState : [LineSegment.LineSegment] -> State
initialState = concatMap (generateTreasure Normal 1.0)

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
