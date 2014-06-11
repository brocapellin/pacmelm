module Gfx where

renderResult :
    { a
      | x : { b
              | z : Int
            }
    }
 -> Element
renderResult state = asText state.x.z
