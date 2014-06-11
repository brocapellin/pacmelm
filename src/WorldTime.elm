module WorldTime where

import Time

type State = Float

initialState = 0.0

moment = fps 25

captureMoment timeDelta = 
    { timeDelta = timeDelta
    }

newState moment state = state + inSeconds moment.timeDelta 
