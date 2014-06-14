module WorldTime where

import Time

type State = Float

initialState : State
initialState = 0.0

moment : Signal Time
moment = fps 25

type Moment =
    { timeDelta : Time
    }

captureMoment : Time -> Moment 
captureMoment timeDelta = 
    { timeDelta = timeDelta
    }

newState : Moment -> State -> State
newState moment state = state + inSeconds moment.timeDelta 
