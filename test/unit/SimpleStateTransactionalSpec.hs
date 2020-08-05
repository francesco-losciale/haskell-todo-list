module SimpleStateTransactionalSpec where

import Test.Hspec
import StateMonadSpec

data TransactionalStep = Begin | Commit | Rollback | Abort deriving Eq
type TransactionalState = (Integer, Integer)

begin :: State TransactionalState TransactionalStep
begin = do 
          return(Begin)

commit :: State TransactionalState TransactionalStep
commit = do 
          getSt 
          return(Commit)          

rollback :: State TransactionalState TransactionalStep -> State TransactionalState TransactionalStep
rollback state = do
          (s,s') <- getSt      
          if runState state (s,s') /= (Begin, (s,s')) then
            return(Abort)
          else 
            do
            putSt (s',s')
            return(Rollback) 

test_Addition :: (Integer -> Integer) -> State TransactionalState TransactionalStep
test_Addition f = do 
            (s, s') <- getSt
            case s of
              1 -> begin
              2 -> putSt ((f s), s') >> commit
              3 -> putSt ((f s), s') >> rollback 

spec = do
  describe "SimpleStateTransactional: basic functions" $ do
      it "Can begin transactions" $ do
        runState (test_Addition (+1)) (1,1) == (Begin,(1,1))       
      it "Can commit transactions" $ do
        runState (test_Addition (+1)) (2,2) == (Commit,(3,2))  
      it "Can rollback transactions" $ do
        runState (test_Addition (+1)) (3,3) == (Rollback,(3,3))           
      it "Can begin, rollback and commit transactions" $ do
        runState (func) (3,3) == (Commit,(2,2))              
      it "Can't rollback when transactions not began" $ do
        runState (invFunc) (3,3) == (Abort,(3,3))   
  where 
    func = do 
            begin
            putSt (1,1)
            rollback
            begin 
            putSt (2,2)
            commit
    invFunc = do 
            putSt (1,1)
            rollback