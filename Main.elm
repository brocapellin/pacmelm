import Window
import Time

main = renderLoop

renderLoop =
  let
    newState     = newTime
    initialState = { time = 0.0 }
    runWorldAt   = foldp newState initialState
  in
    lift2 renderResult Window.dimensions
        <| runWorldAt
        <| fps 25

newTime deltaTime oldState =
    { oldState | time <- oldState.time + inSeconds deltaTime }

renderResult (windowWidth, windowHeight) newState =
    collage windowWidth windowHeight
        <| resultObjects windowWidth windowHeight newState

resultObjects windowWidth windowHeight state =
  let
    unit       = 300.0
    background = filled black
                    <| rect (toFloat windowWidth) (toFloat windowHeight)
  in
    [ background
    , pacman unit state
    ]

pacman unit state = 
  let
    bodySize       = unit
    body           = filled lightYellow
                        <| circle (bodySize*0.5)
    halfMouthSize  = 0.6 -- higher is wider
    eatingSpeed    = 0.4 -- lower is faster
    mouthEnds      = state.time `fmod` eatingSpeed
                        * halfMouthSize / eatingSpeed * bodySize
    mouth      = filled black
                    <| polygon
                       [ (-bodySize*0.5, mouthEnds)
                       , (-bodySize*0.5, -mouthEnds)
                       , (bodySize*0.25, 0.0)
                       ]
  in
    group
    [ body
    , mouth
    ]

fmod x y =
    x - (toFloat (floor (x / y))) * y
