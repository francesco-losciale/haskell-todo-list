module StateSpec where

import Test.Hspec

-- Haskell record type that wraps the state transformer.
-- The type of `runState` is :
--  State state result -> (state -> (result, state))
newtype State state result = State {
    runState :: state -> (result, state)
}

returnSt :: result -> State state result
returnSt result = State $ \state -> (result, state)

bindSt :: (State state result) -> (result -> State state newResult) -> (State state newResult)
bindSt transformerWrapper calculateResultAndInjectInState =  State $
        \state -> let (result, newState) = runState transformerWrapper state
                  in runState (calculateResultAndInjectInState result) newState

getSt :: State state state
getSt = State $ \state -> (state, state)

putSt :: state -> State state ()
putSt state = State $ \_ -> ((), state)

spec :: Spec
spec = do
  describe "State: example of a state transformer in a wrapper" $ do
      it "Given separate value and state, apply state transformer" $ do
        (runState (returnSt value) state) == (value, state)
      it "Given a state, transform the value but not the state" $ do
        runState ((returnSt value) `bindSt` calculateResultAndInjectInState) state == ("result", state)
      it "Given a state, get a transformer wrapper out of it" $ do
        runState getSt state == (state, state)
      it "Given a state, ignore it and replace with a new one" $ do
        runState (putSt 2) state == ((), 2)
      it "Given a state, get a transformer wrapper out of it and bind with putSt" $ do
        runState (getSt `bindSt` putSt) state == ((), 1)
      it "Given a state, get a transformer wrapper out of it and bind with function that changes state" $ do
        runState (getSt `bindSt` (\_ -> putSt 2)) state == ((), 2)
      it "Given a state, get a transformer wrapper, change the state, calculate a result" $ do
        runState (getSt `bindSt` (\_ -> putSt 2) `bindSt` calculateResultAndInjectInState) state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInState = \value -> returnSt("result")
