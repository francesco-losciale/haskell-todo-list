module State.StateTransMonadSpec where

import Test.Hspec

import State.TransactionalStateMonad

spec :: Spec
spec = do
  describe "StateTransMonad: example of a state monad" $ do
      it "Can rollback state changes" $ do
        runState rollbackTest state == ("result", (1,1))
      it "Can commit state changes" $ do
        runState commitTest state == ("result", (2,3)) -- (committed state, transactional state)
  where
    value = "value"
    state = buildTs 1
    rollbackTest = do { putTs 2; rollbackTs; return("result")}
    -- commitTest = do { putTs 2; commitTs; return("result"); putTs 3; return("result")}
    commitTest = return("result") >>= \_ -> putTs 2 >>= \s -> commitTs >>= \s -> return("result") >>= \s -> putTs 3 >>= \s -> return("result")