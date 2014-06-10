module Input where

import Keyboard



initialState completeState =
    { completeState
        | input = { wasd = { x = 0
                           , y = 0
                           }
                  , move = { x = 0.0
                           , y = 0.0
                           }
                  }
    }

moment = Keyboard.wasd

captureMoment wasd moment =
    { moment
        | input = { wasd = wasd
                  }
    }

newState moment = newMove moment

newMove moment state =
  let
    (moveX, moveY) = if
        | moment.input.wasd.x == state.input.wasd.x
            && moment.input.wasd.y == state.input.wasd.y
            -> (state.input.move.x, state.input.move.y)
        | moment.input.wasd.x == 0
            || moment.input.wasd.y == 0
            -> (toFloat moment.input.wasd.x
               ,toFloat moment.input.wasd.y
               )
        | state.input.move.x == 0
            -> (toFloat moment.input.wasd.x, 0.0)
        | otherwise
            -> (0.0, toFloat moment.input.wasd.y)
  in 
    { state
        | input = { wasd = moment.input.wasd
                  , move = { x = moveX
                           , y = moveY
                           }
                  }
    }
