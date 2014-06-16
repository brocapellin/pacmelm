module Pacman where

import Point
import Orientation

type Pacman =
    { position : Point.Point
    , orientation : Orientation.Orientation
    }

size : Float
size = 0.5

pacman
  : Point.Point
 -> Orientation.Orientation
 -> Pacman 
pacman position orientation =
    { position
        = position

    , orientation
        = orientation
    }

orient : Pacman -> Orientation.Orientation -> Pacman
orient pacman orientation =
    { pacman
    | orientation
        <- orientation
    }

position : Pacman -> Point.Point -> Pacman
position pacman position =
    { pacman
    | position
        <- position
    }
