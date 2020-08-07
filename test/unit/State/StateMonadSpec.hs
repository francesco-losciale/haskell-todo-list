module State.StateMonadSpec where

import Test.Hspec

import State.StateMonad

spec :: Spec
spec = do
  describe "StateMonad: example of a state monad" $ do
      it "Applies state transformer on separate value and state" $ do
        (runState (return value) state) == (value, state)
      it "Gets a State, changes the state, calculates a result" $ do
        runState stateFromDoBlock state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInStateMonad = \value -> return("result")
    stateFromDoBlock = do
        getSt
        putSt 2
        calculateResultAndInjectInStateMonad "value"
