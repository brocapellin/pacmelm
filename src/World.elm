module World where

import WorldTime
import Input

import Point
import Point (point)

import LineSegment
import LineSegment (lineSegment)

import Portal
import Portal (portal)



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
    oldPacman
      = state.pacman

    newPosition
      = (warp . constrainToPath newOrientation)
            (point (oldPosition.x
                    + state.input.move.x * velocity)
                   (oldPosition.y
                    + state.input.move.y * velocity)
            )
             

    newOrientation
      = case (gameX,gameY) of
        (0, 1)    -> Up
        (0,-1)    -> Down
        (1,0)     -> Right
        (-1,0)    -> Left
        otherwise -> oldPacman.orientation

    oldPosition
      = oldPacman.position

    (gameX,gameY)
      = ( round state.input.move.x
        , round state.input.move.y
        )

    velocity
      = 0.3

  in
    { state
        | pacman <- { oldPacman
            | position    <- newPosition
            , orientation <- newOrientation
          }
    }

constrainToPath orientation point =
  let
    distancePoint
      = LineSegment.distancePoint point

    withDistance
      = map (\l -> (distancePoint l,l))

    axisOrientation
      = orientationAxis orientation

    sameAxis
      = filter (\(_,l) -> l.axis == axisOrientation)

    withinRange
      = filter (\(d,_) -> d <= 0.5)

    sortByDistance
      = sortBy fst

    allSortedByDistance
      = (sortByDistance . withDistance) pathSegments

    priorityPaths
      = (withinRange . sameAxis) allSortedByDistance

    target
      = (snd . head) (priorityPaths ++ allSortedByDistance)
  in
    LineSegment.constrain point target

pathSegments =
    [ lineSegment (point -5.0 -5.0) 10.0 LineSegment.X
    , lineSegment (point -5.0 5.0) 10.0 LineSegment.X
    , lineSegment (point -5.0 -5.0) 10.0 LineSegment.Y
    , lineSegment (point 5.0 -5.0) 10.0 LineSegment.Y
    , lineSegment (point -10.0 0.0) 5.0 LineSegment.X
    , lineSegment (point 5.0 0.0) 5.0 LineSegment.X
    ] 

warp = Portal.warp portals

portals      =
    [ portal 0.1 (point -10.0 0.0) (point 9.8 0.0)
    , portal 0.1 (point 10.0 0.0) (point -9.8 0.0)
    ]

data Orientation = Left | Up | Right | Down

orientationAxis orientation =
    if orientation == Left || orientation == Right
    then LineSegment.X
    else LineSegment.Y
