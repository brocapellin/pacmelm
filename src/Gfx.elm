module Gfx where

import World

import Point
import Point (point)

import LineSegment
import LineSegment (lineSegment)

import Orientation
import Axis


{- Why can Elm not infer the type of 'state' without the following signature?
 - Type error without signature:

[4 of 5] Compiling Gfx                 ( Gfx.elm )
[5 of 5] Compiling Main                ( Main.elm )
Type error between lines 5 and 6:
        (Gfx.renderResult <~ Window.dimensions) ~ World.world

   Expected Type: {}
     Actual Type: {input : {move : {x : Float, y : Float},
                            wasd : {x : Int, y : Int}}} 
 -}
renderResult :
    (Int, Int)
 -> World.State
 -> Element 
renderResult (windowWidth, windowHeight) state =
    resultObjects windowWidth windowHeight state
        |> collage windowWidth windowHeight

resultObjects windowWidth windowHeight state =
  let
    background = rect (toFloat windowWidth)
                      (toFloat windowHeight)
                    |> filled black
  in
       [ background ]
    ++ map levelPart World.pathSegments
    ++ [ pacman state ]

pacman state = 
  let
    body          = circle (bodySize*0.5)
                        |> filled lightYellow 
    mouth         = polygon
                    [ (-bodySize*0.6, mouthEnds)
                    , (-bodySize*0.6, -mouthEnds)
                    , (bodySize*0.25, 0.0)
                    ]
                        |> filled black
    rotation      = case state.pacman.orientation of
        Orientation.West  -> 0
        Orientation.South -> 90
        Orientation.East  -> 180
        Orientation.North -> 270

    bodySize      = toPixels 1.0
    mouthEnds     = state.worldTime `fmod` eatingSpeed
                       * halfMouthSize / eatingSpeed
                       * bodySize
    halfMouthSize = 0.6 -- higher is wider
    eatingSpeed   = 0.4 -- lower is faster
  in
    group
    [ body
    , mouth
    ]
        |> move (toPixels state.pacman.position.x,
                 toPixels state.pacman.position.y)
        |> rotate (degrees rotation)

toPixels units = 40.0 * units

fmod x y =
    x - (toFloat (floor (x / y))) * y

levelPart segment =
  let
    part  = case segment.axis of
        Axis.Y -> rect 1.0 (toPixels segment.length)
        Axis.X -> rect (toPixels segment.length) 1.0

    (x,y) = case segment.axis of
        Axis.Y -> ( segment.start.x
                  , segment.start.y
                    + segment.length * 0.5
                  )
        Axis.X -> ( segment.start.x
                    + segment.length * 0.5
                  , segment.start.y
                  )
  in
    part
        |> filled white
        |> move (toPixels x
                ,toPixels y)
