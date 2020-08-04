module SimpleStateTransactionalSpec where

import Test.Hspec
import StateMonadSpec

data TransactionalStep = Begin deriving Eq

begin :: (Integer -> Integer) -> State Integer TransactionalStep
begin f = do 
            putSt (f 1)
            return(Begin)

spec = do
  describe "SimpleStateTransactional: basic functions" $ do
      it "Takes function and state, executes function and return transaction status" $ do
        runState (begin (+1)) 1 == (Begin,2)       
