module StateMonadSpec where


import Test.Hspec

-- Haskell record type that wraps the state transformer.
-- The type of `runState` is :
--  State state result -> (state -> (result, state))
newtype State state result = State {
    runState :: state -> (result, state)
}

instance Functor (State state) where
    function `fmap` state = do {
            value <- state;
            returnSt (function value)
    }

instance Applicative (State state) where
    pure = returnSt
    functionState <*> state = do {
        function <- functionState;
        value <- state;
        return (function value)
    }

instance Monad (State state) where
    return = returnSt
    state >>= function = state `bindSt` function

returnSt :: result -> State state result
returnSt result = State $ \state -> (result, state)

bindSt :: (State state result) -> (result -> State state newResult) -> (State state newResult)
bindSt transformerWrapper calculateResultAndInjectInStateMonad =  State $
        \state -> let (result, newState) = runState transformerWrapper state
                  in runState (calculateResultAndInjectInStateMonad result) newState

getSt :: State state state
getSt = State $ \state -> (state, state)

putSt :: state -> State state ()
putSt state = State $ \_ -> ((), state)

spec :: Spec
spec = do
  describe "StateMonad: example of a state transformer" $ do
      it "Given separate value and state, apply state transformer" $ do
        (runState (return value) state) == (value, state)
      it "Given a state, get a SimpleState, change the state, calculate a result" $ do
        runState stateFromDoBlock state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInStateMonad = \value -> return("result")
    stateFromDoBlock = do
        getSt
        putSt 2
        calculateResultAndInjectInStateMonad "value"
