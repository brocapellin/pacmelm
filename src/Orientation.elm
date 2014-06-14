module Orientation where

import Point
import Maybe

data Orientation =
    North
  | East
  | South
  | West

fromPoint : Point.Point -> Maybe Orientation
fromPoint point =
  if | point.y > 0.0 -> Just North
     | point.y < 0.0 -> Just South
     | point.x > 0.0 -> Just East
     | point.x < 0.0 -> Just West
     | otherwise     -> Nothing
