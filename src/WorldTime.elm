module WorldTime where

import Time


initialState completeState =
    { completeState
        | worldTime = 0.0
    }

moment = fps 25

captureMoment timeDelta moment =
    { moment
        | worldTime = { timeDelta = timeDelta
                      }
    }

newState moment = newTime moment 

newTime moment state =
    { state
        | worldTime <- state.worldTime
            + inSeconds moment.worldTime.timeDelta
    }
