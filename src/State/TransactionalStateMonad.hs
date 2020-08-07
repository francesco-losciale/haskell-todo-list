module State.TransactionalStateMonad  (
       runState
     , getTs
     , putTs
     , buildTs
     , commitTs
     , rollbackTs
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

-- t transactional state, s committed state
getTs :: State (s, t) t
getTs = State $ \(s, t) -> (t, (s,t))

putTs :: x -> State (s, x) ()
putTs x = State $ \(s, t) -> ((), (s, x))

buildTs :: s -> (s,s)
buildTs s = (s,s)

commitTs :: State (t, t) ()
commitTs = State $ \(s, t) -> ((), (t, t))

rollbackTs :: State (s, s) ()
rollbackTs = State $ \(s, t) -> ((), (s, s))
