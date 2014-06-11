import Window
import Gfx

main = Gfx.renderResult <~ world

world = foldp newState initialState <| every 1.0

initialState = 
    { x = { z = 0 }
    , y = 0
    }

newState moment state =
    { state
      | x <- { z = floor moment }
    }
