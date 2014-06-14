module Portal where

import Maybe
import Point

import Circle
import Circle (circle)

type Portal =
    Point.Point -> Maybe Point.Point

portal
  : Float
 -> Point.Point
 -> Point.Point
 -> Portal
portal radius a b c = 
    if Circle.intersectsPoint (circle c radius) a
    then Just b
    else Nothing

warp : [Portal] -> Point.Point -> Point.Point
warp portals a =
  let
    portals' = portals ++ [portal 0.01 a a]
  in
    (head . justs) <| fmap a portals'

fmap : a -> [a -> b] -> [b]
fmap a fs =
  let
    as' = repeat (length fs) a
  in
    zipWith (\f a -> f a) fs as'
