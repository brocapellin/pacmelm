import Window
import Time

main = lift2 render Window.dimensions (foldp step state (fps 25))

state = { time = 0.0 }

step deltaTime state = { state | time <- state.time + inSeconds deltaTime }

render (w,h) state = 
  let
    forms      = allForms w h state.time
  in
    collage w h
    [ forms.background
    , forms.pacman
    ] 

allForms w h time =
  let
    unit = 300.0
  in
    { background = background (toFloat w) (toFloat h)
    , pacman     = pacman unit time
    }

background w h = rect w h |> filled black

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
                 [ (-(bodySize * 0.5), mouthEnds)
                 , (-(bodySize * 0.5), -mouthEnds)
                 , (bodySize * 0.25, 0.0)
                 ]
  in
    group
    [ body
    , mouth
    ]

fmod x y = x - (toFloat (floor (x / y))) * y
