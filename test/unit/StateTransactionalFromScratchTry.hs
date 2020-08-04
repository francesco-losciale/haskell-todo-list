module StateTransactionalFromScratchTry where

-- import Control.Monad (when)
-- import Test.Hspec

-- -- | State Monad

-- newtype State state result = State {
--     runState :: state -> (result, state)
-- }

-- instance Functor (State state) where
--     function `fmap` state = do {
--             value <- state;
--             return (function value)
--     }

-- instance Applicative (State state) where
--     pure = return
--     functionState <*> state = do {
--         function <- functionState;
--         value <- state;
--         return (function value)
--     }

-- instance Monad (State state) where
--     return result = State $ \state -> (result, state)
--     (>>=) transformerWrapper calculateResultAndInjectInStateMonad =  State $
--             \state -> let (result, newState) = runState transformerWrapper state
--                       in runState (calculateResultAndInjectInStateMonad result) newState

-- getSt :: State state state
-- getSt = State $ \state -> (state, state)

-- putSt :: state -> State state ()
-- putSt state = State $ \_ -> ((), state)

-- -- | Transaction Monad

-- newtype TransactionalState state result = TrStCon State (state, state) result


-- test0 :: Int -> TransactionMonad Int ()
-- test0 = do
--   s <- get
--   set 99
--   case s of
--     _ -> commit "wooo"

-- -- test0 :: TransactionHandle o Int String String -> TransactionMonad o Int ()
-- -- begin :: (TransactionHandle o state e result -> TransactionMonad o state ()) -> TransactionMonad o state (TransactionStatus e result)
-- -- runTxM_ :: TransactionMonad (a,s) s a -> (s -> (a, s))
-- spec = do
--   describe "StateTransactionalFromScratchTry: example of a transactional state" $ do
--       it "Input value of 4 to test0 should commit the transaction" $ do
--         (begin test0) 4 == (Commit "wooo",99)
