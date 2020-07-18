{-# LANGUAGE RankNTypes #-}

module SimpleTransactionalStateSpec where

import Control.Monad (when)

import Test.Hspec

-- | Transaction Status.
data TransactionStatus e result
  = Begin
  | Abort (Maybe e)            -- ^ Reverted  state, returned an error.
  | Commit result              -- ^ Committed state, returned a result.
  deriving(Eq,Ord,Read,Show)

-- | A transaction handle.
newtype TransactionHandle o state e result  = TransactionHandle  {unTx :: Lbl o state (TransactionStatus e result)}

-- | The transaction monad. A State monad, with transactional state.
newtype TransactionMonad o s a   = TransactionMonad {unTxM :: s -> (s -> a -> o) -> o}

instance Applicative (TransactionMonad o s) where
  pure = return
  fs <*> s = do {f <- fs; v <- s; return (f v) }
instance Functor (TransactionMonad o s) where
  fmap f (TransactionMonad g) = TransactionMonad (\s k -> g s (\s a -> k s (f a)))
instance Monad (TransactionMonad o s) where
  return a = TransactionMonad (\s k -> k s a)
  TransactionMonad g >>= f = TransactionMonad (\s k -> g s (\s a -> unTxM (f a) s k))

newtype Lbl o s a   = Lbl {unLbl :: (a, Lbl o s a) -> TransactionMonad o s ()}

set :: s -> TransactionMonad o s ()
set s = TransactionMonad (\_ k -> k s ())

get :: TransactionMonad o s s
get = TransactionMonad (\s k -> k s s)

-- | Begin a transaction. @begin@ takes a function
-- withCommit :: ((forall b. result -> TransactionMonad o s b) -> TransactionMonad o s result) -> TransactionMonad o s result
begin :: (TransactionHandle o state e result -> TransactionMonad o state ()) -> TransactionMonad o state (TransactionStatus e result)
begin f = withCommit (\abort -> do
            (transactionStatus, lbl) <- checkpoint
            when (isBegin transactionStatus)
              (f (TransactionHandle lbl) >> abort (Abort Nothing))
            return transactionStatus)

-- | Commit state, return a result.
commit   :: TransactionHandle o state e result ->       result -> TransactionMonad o state ()
commit    (TransactionHandle lbl) result = jump lbl (Commit result)

isBegin :: TransactionStatus e result -> Bool
isBegin Begin = True
isBegin _     = False

-- withCommit :: ((forall b. result -> TransactionMonad o s b) -> TransactionMonad o s result) -> TransactionMonad o s result
checkpoint :: TransactionMonad o s (TransactionStatus e result, Lbl o s (TransactionStatus e result))
checkpoint = withCommit (\commit ->
  let go (Begin,      lbl) = error "TODO: nested transactions?"
      go (Commit result,   lbl) = commit    (Commit result,    lbl)
  in return (Begin, Lbl go))

-- newtype TransactionMonad o s a   = TransactionMonad {unTxM :: s -> (s -> a -> o) -> o
withCommit :: ((forall b. result -> TransactionMonad o state b) -> TransactionMonad o state result) -> TransactionMonad o state result
withCommit f = TransactionMonad (\state k -> unTxM (f (\result -> TransactionMonad (\state _ -> k state result))) state k)

jump :: Lbl o s a -> a -> TransactionMonad o s b
jump (Lbl k) a = k (a, Lbl k) >> undefined


runTxM_ :: TransactionMonad (a,s) s a -> s -> (a, s)
runTxM_ (TransactionMonad g) s = g s (\s a -> (a, s))

-- commit   :: TransactionHandle o state e result ->       result -> TransactionMonad o state ()
-- nota bene: the transactionHandle is passed in the begin function expression
test0 :: TransactionHandle o Int String String -> TransactionMonad o Int ()
test0 transactionHandle = do
  s <- get
  set 99
  case s of
    _ -> commit transactionHandle "wooo"

-- test0 :: TransactionHandle o Int String String -> TransactionMonad o Int ()
-- begin :: (TransactionHandle o state e result -> TransactionMonad o state ()) -> TransactionMonad o state (TransactionStatus e result)
-- runTxM_ :: TransactionMonad (a,s) s a -> (s -> (a, s))
spec = do
  describe "SimpleTransactionalState: example of a transactional state" $ do
      it "Input value of 4 to test0 should commit the transaction" $ do
        (runTxM_ (begin test0)) 4 == (Commit "wooo",99)
