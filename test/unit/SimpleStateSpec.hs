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

getSt :: SimpleState state state
getSt = \state -> (state, state)

putSt :: state -> SimpleState state ()
putSt state = \_ -> ((), state)

spec :: Spec
spec = do
  describe "Simple example of a state transformer" $ do
      it "Given separate value and state, apply state transformer" $ do
        ((returnSt value) state) == (value, state)
      it "Given a state, transform the value but not the state" $ do
        ((returnSt value) `bindSt` calculateResultAndInjectInSimpleState) state == ("result", state)
      it "Given a state, get a SimpleState out of it" $ do
        getSt state == (state, state)
      it "Given a state, ignore it and replace with a new one" $ do
        putSt 2 state == ((), 2)
      it "Given a state, get a SimpleState out of it and bind with putSt" $ do
        (getSt `bindSt` putSt) state == ((), 1)
      it "Given a state, get a SimpleState out of it and bind with function that changes state" $ do
        (getSt `bindSt` (\_ -> putSt 2)) state == ((), 2)
      it "Given a state, get a SimpleState, change the state, calculate a result" $ do
        (getSt `bindSt` (\_ -> putSt 2) `bindSt` calculateResultAndInjectInSimpleState) state == ("result", 2)
  where
    value = "value"
    state = 1
    calculateResultAndInjectInSimpleState = \value -> returnSt("result")
