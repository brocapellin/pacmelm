module World where

import WorldTime
import Input

import Point
import Point (point)

import LineSegment
import LineSegment (lineSegment)

import Portal
import Portal (portal)

import Pacman
import Pacman (pacman)

import Orientation
import Axis



world = foldp newState initialState moment

type State =
    { pacman : Pacman.Pacman 
    , input  : Input.State 
    , worldTime : WorldTime.State
    }

initialState : State 
initialState = 
    { pacman    = pacman (point 0.0 0.0) Orientation.West
    , input     = Input.initialState
    , worldTime = WorldTime.initialState
    }

type Moment = 
    { worldTime : WorldTime.Moment
    , input     : Input.Moment 
    }

moment : Signal Moment
moment =
  let
    combine worldTime input =
        { worldTime = WorldTime.captureMoment worldTime
        , input     = Input.captureMoment input
        }
  in
    combine <~ WorldTime.moment ~ Input.moment

newState
  : Moment
 -> State
 -> State
newState moment =
    (positions . orientations . forces moment)

forces
  : Moment
 -> State
 -> State
forces moment state =
    { state
    | worldTime <- WorldTime.newState
                   moment.worldTime state.worldTime
    , input     <- Input.newState
                   moment.input state.input
    }

orientations
  : State
 -> State
orientations state =
  let
    newOrientation
      = case Orientation.fromPoint state.input.move of
          Just orientation -> orientation
          otherwise        -> state.pacman.orientation
  in
    { state
    | pacman <- Pacman.orient state.pacman newOrientation
    }

positions
  : State
 -> State
positions state =
  let
    orientation
      = state.pacman.orientation

    newPosition
      = ( warp
        . constrainToPath orientation
        )
            (point (state.pacman.position.x
                    + state.input.move.x * velocity)
                   (state.pacman.position.y
                    + state.input.move.y * velocity)
            )
             
    velocity
      = 0.3
  in
    { state
    | pacman <- Pacman.position state.pacman newPosition
    }

constrainToPath
  : Orientation.Orientation
 -> Point.Point
 -> Point.Point
constrainToPath orientation point =
  let
    distancePoint
      = LineSegment.distancePoint point

    withDistance
      = map (\l -> (distancePoint l,l))

    axisOrientation
      = axisFromOrientation orientation

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

pathSegments : [LineSegment.LineSegment]
pathSegments =
    [ lineSegment (point -5.0 -5.0) 10.0 Axis.X
    , lineSegment (point -5.0 5.0) 10.0 Axis.X
    , lineSegment (point -5.0 -5.0) 10.0 Axis.Y
    , lineSegment (point 5.0 -5.0) 10.0 Axis.Y
    , lineSegment (point -10.0 0.0) 5.0 Axis.X
    , lineSegment (point 5.0 0.0) 5.0 Axis.X
    ] 

warp : Point.Point -> Point.Point
warp = Portal.warp portals

portals : [Portal.Portal]
portals =
    [ portal 0.1 (point -10.0 0.0) (point 9.8 0.0)
    , portal 0.1 (point 10.0 0.0) (point -9.8 0.0)
    ]

axisFromOrientation : Orientation.Orientation -> Axis.Axis
axisFromOrientation orientation =
  case orientation of
    Orientation.West -> Axis.X
    Orientation.East -> Axis.X
    otherwise        -> Axis.Y
