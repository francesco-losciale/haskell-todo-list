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

getSt :: SimpleState state state
getSt = \state -> (state, state)

putSt :: state -> SimpleState state ()
putSt state = \_ -> ((), state)

spec :: Spec
spec = do
  -- we have to use bindSt in the do block in order to overwrite the hidden >>=
  describe "SimpleState concatenation WITHOUT state change" $ do
      it "Inject value with initial state of 1" $ do
        ((returnSt "value") 1) == ("value", 1)
      it "Bind to the previous another state" $ do
        ((returnSt "value") `bindSt` (\value -> returnSt("value2"))) 1 == ("value2", 1)
  describe "SimpleState set and get" $ do
      it "Get State gets the current state and returns it as the result" $ do
        getSt 1 == (1, 1)
      it "Put State ignores the current state and replaces it with the input one" $ do
        putSt 2 1 == ((), 2)
      it "Get chained to the state transformer" $ do
        (getSt `bindSt` (\_ -> returnSt("value"))) 1 == ("value", 1)

