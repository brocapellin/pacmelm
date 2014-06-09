import Window
import Time
import Keyboard

main = renderLoop

renderLoop =
  let
    newState moment = newPacman moment
                      . newWasd moment
                      . newTime moment
    initialState    = { time   = 0.0
                      , pacman = { position = { x = 0.0
                                              , y = 0.0
                                              }
                                 , orientation = Left
                                 }
                      , wasd   = { realX = 0
                                 , realY = 0
                                 , gameX = 0.0
                                 , gameY = 0.0
                                 }
                      }
    runWorld        = foldp newState initialState
    moment          = combineToMoment
                        <~ fps 25 ~ Keyboard.wasd
  in
    renderResult <~ Window.dimensions ~ runWorld moment

newTime moment state =
    { state
        | time <- state.time + inSeconds moment.timeDelta
    }

newWasd moment state =
  let
    oldWasd       = state.wasd
    (gameX,gameY) = if
        | moment.wasd.x == oldWasd.realX
            && moment.wasd.y == oldWasd.realY
            -> (oldWasd.gameX, oldWasd.gameY)
        | moment.wasd.x == 0 || moment.wasd.y == 0
            -> (toFloat moment.wasd.x, toFloat moment.wasd.y)
        | oldWasd.realX == 0
            -> (toFloat moment.wasd.x, 0.0)
        | otherwise
            -> (0.0, toFloat moment.wasd.y)
  in
    { state
        | wasd <- { oldWasd
            | realX <- moment.wasd.x
            , realY <- moment.wasd.y
            , gameX <- gameX
            , gameY <- gameY
          }
    }

newPacman moment state =
  let
    oldPacman      = state.pacman

    newPosition    = { oldPosition
        | x <- oldPosition.x
               + state.wasd.gameX * velocity
        , y <- oldPosition.y
               + state.wasd.gameY * velocity
        }

    newOrientation = case (gameX,gameY) of
        (0, 1)    -> Up
        (0,-1)    -> Down
        (1,0)     -> Right
        (-1,0)    -> Left
        otherwise -> oldPacman.orientation

    oldPosition    = oldPacman.position
    (gameX, gameY) = ( round state.wasd.gameX
                     , round state.wasd.gameY
                     )
    velocity       = unit / 3.0

  in
    { state
        | pacman <- { oldPacman
            | position    <- newPosition
            , orientation <- newOrientation
          }
    }

combineToMoment timeDelta wasd =
    { timeDelta = timeDelta
    , wasd      = wasd
    }

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
    , pacman unit state
    ]

pacman unit state = 
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
        Left  -> 0
        Down  -> 90
        Right -> 180
        Up    -> 270

    bodySize      = unit
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

unit       = 50.0

data Orientation = Left | Up | Right | Down

fmod x y =
    x - (toFloat (floor (x / y))) * y
