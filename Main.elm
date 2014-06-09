import Window
import Time

main = renderLoop

renderLoop =
  let
    newState     = newTime
    initialState = { time = 0.0 }
    runWorldAt   = foldp newState initialState
  in
    renderResult <~ Window.dimensions ~ (runWorldAt . fps) 25

newTime deltaTime oldState =
    { oldState | time <- oldState.time + inSeconds deltaTime }

renderResult (windowWidth, windowHeight) newState =
    resultObjects windowWidth windowHeight newState
        |> collage windowWidth windowHeight

resultObjects windowWidth windowHeight state =
  let
    unit       = 300.0
    background = rect (toFloat windowWidth) (toFloat windowHeight)
                    |> filled black
  in
    [ background
    , pacman unit state
    ]

pacman unit state = 
  let
    bodySize      = unit
    body          = circle (bodySize*0.5)
                        |> filled lightYellow 
    halfMouthSize = 0.6 -- higher is wider
    eatingSpeed   = 0.4 -- lower is faster
    mouthEnds     = state.time `fmod` eatingSpeed
                       * halfMouthSize / eatingSpeed * bodySize
    mouth         = polygon
                    [ (-bodySize*0.5, mouthEnds)
                    , (-bodySize*0.5, -mouthEnds)
                    , (bodySize*0.25, 0.0)
                    ]
                        |> filled black
  in
    group
    [ body
    , mouth
    ]

fmod x y =
    x - (toFloat (floor (x / y))) * y
