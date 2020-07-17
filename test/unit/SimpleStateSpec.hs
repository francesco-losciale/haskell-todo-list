module SimpleStateSpec where

import Test.Hspec

-- state, result are only type parameters here!
type SimpleState state result = state -> (result, state)

returnSt :: result -> SimpleState state result
returnSt result = \state -> (result, state)

bindSt :: (SimpleState state result) -> (result -> SimpleState state newResult) -> (SimpleState state newResult)
bindSt simpleState calculateNewSimpleState =
        \oldState -> let (result, newState) = simpleState oldState
                     in (calculateNewSimpleState result) newState

spec :: Spec
spec = do
  describe "SimpleState concatenation without state change" $ do
      it "Inject value with initial state of 1" $ do
        ((returnSt "value") 1) == ("value", 1)
      it "Bind to the previous another state" $ do
        ((returnSt "value") `bindSt` (\value -> returnSt("value2"))) 1 == ("value2", 1)
