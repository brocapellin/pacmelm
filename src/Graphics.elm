module Graphics where

import World

renderResult (windowWidth, windowHeight) newState =
    resultObjects windowWidth windowHeight newState
        |> collage windowWidth windowHeight

resultObjects windowWidth windowHeight state =
  let
    background = rect (toFloat windowWidth)
                      (toFloat windowHeight)
                    |> filled black
  in
    [ background
    , pacman state
    ]

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
        World.Left  -> 0
        World.Down  -> 90
        World.Right -> 180
        World.Up    -> 270

    bodySize      = World.unit
    mouthEnds     = state.time `fmod` eatingSpeed
                       * halfMouthSize / eatingSpeed
                       * bodySize
    halfMouthSize = 0.6 -- higher is wider
    eatingSpeed   = 0.4 -- lower is faster
  in
    group
    [ body
    , mouth
    ]
        |> move (state.pacman.position.x,
                 state.pacman.position.y)
        |> rotate (degrees rotation)

fmod x y =
    x - (toFloat (floor (x / y))) * y
