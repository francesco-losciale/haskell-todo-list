module State.State where

-- Haskell record type that wraps the state transformer.
-- The type of `runState` is :
--  State state result -> (state -> (result, state))
newtype State state result = State {
    runState :: state -> (result, state)
}

returnSt :: result -> State state result
returnSt result = State $ \state -> (result, state)

bindSt :: (State state result) -> (result -> State state newResult) -> State state newResult
bindSt transformerWrapper calculateResultAndInjectInState =  State $
        \state -> let (result, newState) = runState transformerWrapper state
                  in runState (calculateResultAndInjectInState result) newState

getSt :: State state state
getSt = State $ \state -> (state, state)

putSt :: state -> State state ()
putSt state = State $ \_ -> ((), state)
