module Axis where

data Axis =
    X
  | Y

mirror : Axis -> Axis
mirror axis = if axis == X then Y else X
