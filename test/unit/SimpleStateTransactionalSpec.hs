module SimpleStateTransactionalSpec where

import Test.Hspec
import StateMonadSpec

data TransactionalStep = Begin | Commit deriving Eq

begin :: State Integer TransactionalStep
begin = do 
          getSt 
          return(Begin)

commit :: State Integer TransactionalStep
commit = do 
          getSt 
          return(Commit)          

atomically :: (Integer -> Integer) -> State Integer TransactionalStep
atomically f = do 
            s <- getSt
            case s of
              1 -> putSt (f s) >> begin
              2 -> putSt (f s) >> commit


spec = do
  describe "SimpleStateTransactional: basic functions" $ do
      it "Can open transactions" $ do
        runState (atomically (+1)) 1 == (Begin,1)       
      it "Can commit transactions" $ do
        runState (atomically (+1)) 2 == (Commit,3)           
   
   
