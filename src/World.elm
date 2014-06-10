module World where

import WorldTime
import Input



runWorld = foldp newState initialState moment

initialState = 
    ( WorldTime.initialState . Input.initialState )
    { pacman = { position = { x = 0.0
                            , y = 0.0
                            }
               , orientation = Left
               }
    }

moment =
  let
    captureMoment worldTime input =
        ( WorldTime.captureMoment worldTime
        . Input.captureMoment input ) {}
  in
    captureMoment <~ WorldTime.moment ~ Input.moment

newState moment = newPacman 
                  . WorldTime.newState moment
                  . Input.newState moment

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
