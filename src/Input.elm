module Input where

import Keyboard


type State a b c =
    { a
      | wasd : { b
                 | x : Int
                 , y : Int
               }
      , move : { c
                 | x : Float
                 , y : Float
               }
    }

initialState =
    { wasd = { x = 0
             , y = 0
             }
    , move = { x = 0.0
             , y = 0.0
             }
    }

moment = Keyboard.wasd

captureMoment wasd =
    { wasd = wasd
    }

newState moment state =
  let
    (moveX, moveY) = if
        | moment.wasd.x == state.wasd.x
            && moment.wasd.y == state.wasd.y
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
    , move = { x = moveX
             , y = moveY
             }
    }
