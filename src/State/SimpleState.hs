module State.SimpleState where

-- aka state transformer
type SimpleState state result = state -> (result, state)

returnSt :: result -> SimpleState state result
returnSt result = \state -> (result, state)

bindSt :: (SimpleState state result) -> (result -> SimpleState state newResult) -> (SimpleState state newResult)
bindSt simpleState calculateResultAndInjectInSimpleState =
        \state -> let (result, newState) = simpleState state
                  in (calculateResultAndInjectInSimpleState result) newState

getSt :: SimpleState state state
getSt = \state -> (state, state)

putSt :: state -> SimpleState state ()
putSt state = \_ -> ((), state)
