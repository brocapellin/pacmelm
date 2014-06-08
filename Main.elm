import Window
import Time

main = lift2 render Window.dimensions (foldp step state (fps 25))

state = { time = 0.0 }

step deltaTime state = { state | time <- state.time + inSeconds deltaTime }

render (w,h) state = collage w h <| allForms w h state.time

allForms w h time =
  let
    unit        = 300.0
    background  = rect (toFloat w) (toFloat h) |> filled black
  in
    [ background
    , pacman unit time
    ]

pacman unit time = 
  let
    bodySize       = unit
    body           = circle (bodySize*0.5)
                        |> filled lightYellow
    halfMouthSize  = 0.6 -- higher is wider
    eatingSpeed    = 0.4 -- lower is faster
    mouthEnds      = time `fmod` eatingSpeed
                        * halfMouthSize / eatingSpeed
                        * bodySize
    mouth      = filled black <| polygon
                 [ (-bodySize*0.5, mouthEnds)
                 , (-bodySize*0.5, -mouthEnds)
                 , (bodySize*0.25, 0.0)
                 ]
  in
    group
    [ body
    , mouth
    ]

fmod x y = x - (toFloat (floor (x / y))) * y
