module SimpleStateTransactionalSpec where

import Test.Hspec
import StateMonadSpec

data TransactionalStep = Begin deriving Eq

begin :: (a -> b) -> a -> (b, TransactionalStep)
begin f x = (f x, Begin)

spec = do
  describe "SimpleStateTransactional: basic functions" $ do
      it "Takes a function as input, begin transaction, executes it and return result" $ do
        begin (+1) 1 == (2, Begin)       
