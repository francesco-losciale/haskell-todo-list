module SimpleStateTransactionalSpec where

import Test.Hspec
import StateMonadSpec

data TransactionalStep = Begin | Commit deriving Eq

begin :: State (Integer, Integer) TransactionalStep
begin = do 
          (s,s') <- getSt
          putSt (s',s') 
          return(Begin)

commit :: State (Integer, Integer) TransactionalStep
commit = do 
          getSt 
          return(Commit)          

atomically :: (Integer -> Integer) -> State (Integer, Integer) TransactionalStep
atomically f = do 
            (s, s') <- getSt
            case s of
              1 -> putSt ((f s), s') >> begin
              2 -> putSt ((f s), s') >> commit


spec = do
  describe "SimpleStateTransactional: basic functions" $ do
      it "Can open transactions" $ do
        runState (atomically (+1)) (1,1) == (Begin,(1,1))       
      it "Can commit transactions" $ do
        runState (atomically (+1)) (2,2) == (Commit,(3,2))           
   
   
