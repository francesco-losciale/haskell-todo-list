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
    (>>=) ma f =  State $
            \s -> let (result, s') = runState ma s
                      in runState (f result) s'

-- t transactional state, s committed state
getSt :: State (s, t) t
getSt = State $ \(s, t) -> (t, (s,t))

putSt :: x -> State (s, x) ()
putSt x = State $ \(s, t) -> ((), (s, x))

buildSt :: s -> (s,s)
buildSt s = (s,s)

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
        runState commitTest state == ("result", (2,3)) -- (committed state, transactional state)
  where
    value = "value"
    state = buildSt 1
    rollbackTest = do { putSt 2; rollbackSt; return("result")}
    -- commitTest = do { putSt 2; commitSt; return("result"); putSt 3; return("result")}
    commitTest = return("result") >>= \_ -> putSt 2 >>= \s -> commitSt >>= \s -> return("result") >>= \s -> putSt 3 >>= \s -> return("result")