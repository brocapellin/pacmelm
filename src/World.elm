module World where

import WorldTime
import Input



world = foldp newState initialState moment

type State a b c d e f =
    { a
      | pacman : { b
                   | position    : { c
                                     | x : Float
                                     , y : Float
                                   }
                   , orientation : Orientation
                 }
      , input : Input.State d e f
      , worldTime : WorldTime.State 
    }

initialState = 
    { pacman = { position = { x = 0.0
                            , y = 0.0
                            }
               , orientation = Left
               }
    , input = Input.initialState
    , worldTime = WorldTime.initialState
    }

moment =
  let
    combine worldTime input =
        { worldTime = WorldTime.captureMoment worldTime
        , input     = Input.captureMoment input
        }
  in
    combine <~ WorldTime.moment ~ Input.moment

newState moment state =
  let
    newState = 
        { state
          | worldTime <- WorldTime.newState
                            moment.worldTime state.worldTime
          , input     <- Input.newState
                            moment.input state.input
        }
  in
    newPacman newState

newPacman state =
  let
    oldPacman      = state.pacman

    newPosition    = { oldPosition
        | x <- oldPosition.x
               + state.input.move.x * velocity
        , y <- oldPosition.y
               + state.input.move.y * velocity
        }

    newOrientation = case (gameX,gameY) of
        (0, 1)    -> Up
        (0,-1)    -> Down
        (1,0)     -> Right
        (-1,0)    -> Left
        otherwise -> oldPacman.orientation

    oldPosition    = oldPacman.position
    (gameX, gameY) = ( round state.input.move.x
                     , round state.input.move.y
                     )
    velocity       = unit / 3.0

  in
    { state
        | pacman <- { oldPacman
            | position    <- newPosition
            , orientation <- newOrientation
          }
    }

unit       = 50.0

data Orientation = Left | Up | Right | Down