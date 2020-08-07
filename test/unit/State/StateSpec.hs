module State.StateSpec where

import Test.Hspec

import State.State

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
