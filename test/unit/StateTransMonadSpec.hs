module StateTransMonadSpec where

import Test.Hspec

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

-- t transactional state, s committed state, r result
getSt :: State (s, t) t
getSt = State $ \(s, t) -> (t, (s,t))

putSt :: x -> State (s, x) ()
putSt x = State $ \(s, t) -> ((), (s, x))

commitSt :: State (t, t) ()
commitSt = State $ \(s, t) -> ((), (t, t))

rollbackSt :: State (s, s) ()
rollbackSt = State $ \(s, t) -> ((), (s, s))

---------------------------------------------------

spec :: Spec
spec = do
  describe "StateTransMonad: example of a state monad" $ do
      it "Can rollback state changes" $ do
        runState rollbackTest state == ("result", (1,1))
      it "Can commit state changes" $ do
        runState rollbackTest state == ("result", (1,1))
  where
    value = "value"
    state = (1,1)
    rollbackTest = do { putSt 2; rollbackSt; return("result")}
    commitTest = do { putSt 2; commitSt; return("result")}