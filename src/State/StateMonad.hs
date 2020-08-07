module State.StateMonad (
       runState
     , getSt
     , putSt
) where

-- Haskell record type that wraps the state transformer.
-- The type of `runState` is :
--  State state result -> (state -> (result, state))
newtype State state result = State {
    runState :: state -> (result, state)
}

instance Functor (State state) where
    function `fmap` state = do {
            value <- state;
            return (function value)
    }

instance Applicative (State state) where
    pure = return
    functionState <*> state = do {
        function <- functionState;
        value <- state;
        return (function value)
    }

instance Monad (State state) where
    return result = State $ \state -> (result, state)
    (>>=) transformerWrapper calculateResultAndInjectInStateMonad =  State $
            \state -> let (result, newState) = runState transformerWrapper state
                      in runState (calculateResultAndInjectInStateMonad result) newState

getSt :: State state state
getSt = State $ \state -> (state, state)

putSt :: state -> State state ()
putSt state = State $ \_ -> ((), state)
