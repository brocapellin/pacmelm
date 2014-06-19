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

import Circle
import Circle (circle)

import Treasure



world = foldp newState initialState moment

type State =
    { pacman : Pacman.Pacman 
    , input  : Input.State 
    , worldTime : WorldTime.State
    , treasure : Treasure.State
    , score : Int
    }

initialState : State 
initialState = 
    { pacman    = pacman (point 0.0 0.0) Orientation.West
    , input     = Input.initialState
    , worldTime = WorldTime.initialState
    , treasure  = Treasure.initialState treasurePathSegments
    , score     = 0
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
    (treasure . positions . orientations . forces moment)

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

treasure
  : State
 -> State
treasure state = 
  let
    collide t =
      Circle.intersectsPoint
        (circle state.pacman.position (Pacman.size * 0.5))
        t.position

    (colliding,remaining) = partition collide state.treasure

    points = (sum . map Treasure.points) colliding

  in
    { state
    | treasure <- remaining
    , score <- state.score + points
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
      = filter (\(d,_) -> d <= Pacman.size)

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

treasurePathSegmentsLeft : [LineSegment.LineSegment]
treasurePathSegmentsLeft =
    [ lineSegment (point -12.5 -14.0) 12.0 Axis.X
    , lineSegment (point -12.5 -13.0) 2.0 Axis.Y
    , lineSegment (point -1.5 -13.0) 2.0 Axis.Y
    , lineSegment (point -11.5 -11.0) 4.0 Axis.X
    , lineSegment (point -4.5 -11.0) 2.0 Axis.X
    , lineSegment (point -10.5 -10.0) 2.0 Axis.Y
    , lineSegment (point -7.5 -10.0) 24.0 Axis.Y
    , lineSegment (point -4.5 -10.0) 2.0 Axis.Y
    , lineSegment (point -11.5 -8.0) 0.0 Axis.X
    , lineSegment (point -6.5 -8.0) 1.0 Axis.X
    , lineSegment (point -3.5 -8.0) 2.0 Axis.X
    , lineSegment (point -12.5 -7.0) 2.0 Axis.Y
    , lineSegment (point -1.5 -7.0) 2.0 Axis.Y
    , lineSegment (point -11.5 -5.0) 3.0 Axis.X
    , lineSegment (point -6.5 -5.0) 4.0 Axis.X
    , lineSegment (point -12.5 7.0) 4.0 Axis.X
    , lineSegment (point -4.5 7.0) 3.0 Axis.X
    , lineSegment (point -12.5 8.0) 3.0 Axis.Y
    , lineSegment (point -4.5 8.0) 2.0 Axis.Y
    , lineSegment (point -11.5 10.0) 3.0 Axis.X
    , lineSegment (point -6.5 10.0) 1.0 Axis.X
    , lineSegment (point -3.5 10.0) 3.0 Axis.X
    , lineSegment (point -12.5 13.0) 1.0 Axis.Y
    , lineSegment (point -1.5 11.0) 3.0 Axis.Y
    , lineSegment (point -11.5 14.0) 3.0 Axis.X
    , lineSegment (point -6.5 14.0) 4.0 Axis.X
    ]

treasurePathSegments : [LineSegment.LineSegment]
treasurePathSegments =
    treasurePathSegmentsLeft
    ++ map LineSegment.mirrorInX treasurePathSegmentsLeft

pathSegments : [LineSegment.LineSegment]
pathSegments =
    [ lineSegment (point -12.5 -14.0) 25.0 Axis.X
    , lineSegment (point -7.5 -8.0) 15.0 Axis.X
    , lineSegment (point -4.5 -2.0) 9.0 Axis.X
    , lineSegment (point -4.5 4.0) 9.0 Axis.X
    , lineSegment (point -12.5 10.0) 25.0 Axis.X
    ]
    ++ pathSegmentsLeft
    ++ map LineSegment.mirrorInX pathSegmentsLeft

pathSegmentsLeft : [LineSegment.LineSegment]
pathSegmentsLeft = 
    [ lineSegment (point -12.5 -14.0) 3.0 Axis.Y
    , lineSegment (point -1.5 -14.0) 3.0 Axis.Y
    , lineSegment (point -12.5 -11.0) 5.0 Axis.X
    , lineSegment (point -4.5 -11.0) 3.0 Axis.X
    , lineSegment (point -10.5 -11.0) 3.0 Axis.Y
    , lineSegment (point -7.5 -11.0) 25.0 Axis.Y
    , lineSegment (point -4.5 -11.0) 3.0 Axis.Y
    , lineSegment (point -12.5 -8.0) 2.0 Axis.X
    , lineSegment (point -12.5 -8.0) 3.0 Axis.Y
    , lineSegment (point -1.5 -8.0) 3.0 Axis.Y
    , lineSegment (point -12.5 -5.0) 11.0 Axis.X
    , lineSegment (point -4.5 -5.0) 9.0 Axis.Y
    , lineSegment (point -14.5 1.0) 10.0 Axis.X
    , lineSegment (point -1.5 4.0) 3.0 Axis.Y
    , lineSegment (point -12.5 7.0) 5.0 Axis.X
    , lineSegment (point -4.5 7.0) 3.0 Axis.X
    , lineSegment (point -12.5 7.0) 7.0 Axis.Y
    , lineSegment (point -4.5 7.0) 3.0 Axis.Y
    , lineSegment (point -1.5 10.0) 4.0 Axis.Y
    , lineSegment (point -12.5 14.0) 11.0 Axis.X
    ]

warp : Point.Point -> Point.Point
warp = Portal.warp portals

portals : [Portal.Portal]
portals =
    [ portal 0.1 (point -14.5 1.0) (point 13.5 1.0)
    , portal 0.1 (point 14.5 1.0) (point -13.5 1.0)
    ]

axisFromOrientation : Orientation.Orientation -> Axis.Axis
axisFromOrientation orientation =
  case orientation of
    Orientation.West -> Axis.X
    Orientation.East -> Axis.X
    otherwise        -> Axis.Y
