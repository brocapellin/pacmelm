module Input where

import Keyboard

import Point
import Point (point)

type State =
    { wasd : { x : Int
             , y : Int
             }
    , move : Point.Point 
    }

initialState : State
initialState =
    { wasd = { x = 0
             , y = 0
             }
    , move = point 0.0 0.0
    }

moment : Signal { x : Int, y : Int }
moment = Keyboard.wasd

type Moment =
    { wasd : { x : Int
             , y : Int
             }
    }

captureMoment
  : { x : Int
    , y : Int
    } 
 -> Moment
captureMoment wasd =
    { wasd = wasd
    }

newState : Moment -> State -> State 
newState moment state =
  let
    wasd' = state.wasd
    move' = state.move
    
    (moveX, moveY) = if
        | (moment.wasd.x == state.wasd.x
           &&
           moment.wasd.y == state.wasd.y
          )
          ||
          (moment.wasd.x == 0 && moment.wasd.y == 0)
            -> (state.move.x, state.move.y)
        | moment.wasd.x == 0 || moment.wasd.y == 0
            -> (toFloat moment.wasd.x
               ,toFloat moment.wasd.y
               )
        | state.move.x == 0
            -> (toFloat moment.wasd.x, 0.0)
        | otherwise
            -> (0.0, toFloat moment.wasd.y)
  in 
    { wasd = moment.wasd
    , move = point moveX moveY
    }
