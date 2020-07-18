module SimpleStateSpec where

import Test.Hspec

-- aka state transformer
type SimpleState state result = state -> (result, state)

returnSt :: result -> SimpleState state result
returnSt result = \state -> (result, state)

bindSt :: (SimpleState state result) -> (result -> SimpleState state newResult) -> (SimpleState state newResult)
bindSt simpleState calculateResultAndInjectInSimpleState =
        \state -> let (result, newState) = simpleState state
                  in (calculateResultAndInjectInSimpleState result) newState

spec :: Spec
spec = do
  describe "Simple example of a state transformer" $ do
      it "Given separate value and state, apply state transformer" $ do
        ((returnSt value) state) == (value, state)
      it "Given a state, transform the value and not the state" $ do
        ((returnSt value) `bindSt` calculateResultAndInjectInSimpleState) state == ("result", state)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInSimpleState = \value -> returnSt("result")
