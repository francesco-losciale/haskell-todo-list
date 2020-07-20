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
      it "Applies state transformer on separate value and state" $ do
        (runState (returnSt value) state) == (value, state)
      it "Transforms the value but not the state" $ do
        runState ((returnSt value) `bindSt` calculateResultAndInjectInState) state == ("result", state)
      it "Gets a SimpleState out of it" $ do
        runState getSt state == (state, state)
      it "Ignores a state and replace it with a new one" $ do
        runState (putSt 2) state == ((), 2)
      it "Gets a SimpleState and binds it with putSt" $ do
        runState (getSt `bindSt` putSt) state == ((), 1)
      it "Gets a SimpleState and binds it with a function that changes state" $ do
        runState (getSt `bindSt` (\_ -> putSt 2)) state == ((), 2)
      it "Gets a State, changes the state, calculates a result" $ do
        runState (getSt `bindSt` (\_ -> putSt 2) `bindSt` calculateResultAndInjectInState) state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInState = \value -> returnSt("result")
