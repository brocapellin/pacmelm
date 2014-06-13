module World where

import WorldTime
import Input

import Point (point)

import LineSegment
import LineSegment (lineSegment)



world = foldp newState initialState moment

initialState = 
    { pacman    = { position    = point 0.0 0.0
                  , orientation = Left
                  }
    , input     = Input.initialState
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

    newPosition    =
        LineSegment.constrainToClosest
            ( point
                (oldPosition.x
                 + state.input.move.x * velocity)
                (oldPosition.y
                 + state.input.move.y * velocity)
            )
            level

    newOrientation = case (gameX,gameY) of
        (0, 1)    -> Up
        (0,-1)    -> Down
        (1,0)     -> Right
        (-1,0)    -> Left
        otherwise -> oldPacman.orientation

    oldPosition    = oldPacman.position
    (gameX,gameY)  = ( round state.input.move.x
                     , round state.input.move.y
                     )
    velocity       = 0.4

  in
    { state
        | pacman <- { oldPacman
            | position    <- newPosition
            , orientation <- newOrientation
          }
    }

data Orientation = Left | Up | Right | Down

level = [ lineSegment (point -5.0 -5.0) 10.0 LineSegment.X
        , lineSegment (point -5.0 5.0) 10.0 LineSegment.X
        , lineSegment (point -5.0 -5.0) 10.0 LineSegment.Y
        , lineSegment (point 5.0 -5.0) 10.0 LineSegment.Y
        ] 
