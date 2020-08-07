module State.SimpleStateSpec where

import Test.Hspec

import State.SimpleState

spec :: Spec
spec = do
  describe "SimpleState: example of a state transformer" $ do
      it "Applies state transformer on separate value and state" $ do
        ((returnSt value) state) == (value, state)
      it "Transforms the value but not the state" $ do
        ((returnSt value) `bindSt` calculateResultAndInjectInSimpleState) state == ("result", state)
      it "Gets a SimpleState out of it" $ do
        getSt state == (state, state)
      it "Ignores a state and replace it with a new one" $ do
        putSt 2 state == ((), 2)
      it "Gets a SimpleState and binds it with putSt" $ do
        (getSt `bindSt` putSt) state == ((), 1)
      it "Gets a SimpleState and binds it with a function that changes state" $ do
        (getSt `bindSt` (\_ -> putSt 2)) state == ((), 2)
      it "Gets a SimpleState, changes the state, calculates a result" $ do
        (getSt `bindSt` (\_ -> putSt 2) `bindSt` calculateResultAndInjectInSimpleState) state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInSimpleState = \value -> returnSt("result")
