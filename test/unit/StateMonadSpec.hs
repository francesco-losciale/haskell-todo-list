module StateMonadSpec where

import Control.Monad

import Test.Hspec

-- We use haskell record syntax to define the type.
-- The type of runState is
--      State state result -> (state -> (result, state))

-- state, result are only type parameters here!
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

bindSt :: State state result -> (result -> State state newResult) -> (State state newResult)
bindSt initialState calculateNewState = State $ \oldState -> let (result, newState) = runState initialState oldState
                                                     in runState (calculateNewState result) newState

getSt :: State state state
getSt = State $ \state -> (state, state)

putSt :: state -> State state ()
putSt state = State $ \_ -> ((), state)

spec :: Spec
spec = do
  -- we have to use bindSt in the do block in order to overwrite the hidden >>=
  describe "StateMonad concatenation WITHOUT state change" $ do
      it "Inject value with initial state of 1" $ do
        runState (returnSt "value") 1 == ("value", 1)
      it "Bind to the previous another state" $ do
        runState ((returnSt "value") >>= (\value -> returnSt("value2"))) 1 == ("value2", 1)
  describe "StateMonad set and get" $ do
      it "Get State gets the current state and returns it as the result" $ do
        runState getSt 1 == (1, 1)
      it "Put State ignores the current state and replaces it with the input one" $ do
        (runState (putSt 2)) 1 == ((), 2)
      it "Get chained to the state transformer" $ do
        runState (getSt >>= (\_ -> returnSt("value"))) 1 == ("value", 1)
      it "Overwrite the state using put" $ do
        runState (getSt >>= (\_ -> putSt 2) >>= (\_ -> returnSt("value"))) 1 == ("value", 2)
      it "Overwrite the state using put changing order - the value is not passed by the putSt" $ do
        runState (getSt >>= (\_ -> returnSt("value")) >>= (\_ -> putSt 2)) 1 == ((), 2)
      it "Overwrite the state using put changing order - do something more" $ do
        runState (getSt >>= (\_ -> returnSt("value")) >>= (\value -> returnSt(take 1 value))) 1 == ("v", 1)
  describe "Can use do block now" $ do
      it "Overwrite the state using put changing order - do something more" $ do
        runState exec 1 == ("v", 1)
        where exec = do getSt
                        return (take 1 "value")


