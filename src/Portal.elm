module Portal where

import Maybe
import Point

import Circle
import Circle (circle)

type Portal a =
    Point.Point a -> Maybe Point.Point a

portal radius a b c = 
    if Circle.intersectsPoint (circle c radius) a
    then Just b
    else Nothing

warp portals a =
  let
    portals' = portals ++ [portal 0.01 a a]
  in
    (head . justs) <| fmap a portals'

fmap a fs =
  let
    as' = repeat (length fs) a
  in
    zipWith (\f a -> f a) fs as'
