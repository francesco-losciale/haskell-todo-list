{-# LANGUAGE RankNTypes #-}

module SimpleTransactionalStateSpec where

import Control.Monad (when)

import Test.Hspec

-- | Transaction Status.
data TransactionStatus e a
  = Begin
  | Abort (Maybe e)       -- ^ Reverted  state, returned an error.
  | Commit a              -- ^ Committed state, returned a result.
  deriving(Eq,Ord,Read,Show)

-- | A transaction handle.
newtype TransactionHandle o state e a  = TransactionHandle  {unTx :: Lbl o state (TransactionStatus e a)}

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
--  which represents this transaction.
begin :: (TransactionHandle o state e a -> TransactionMonad o state ()) -> TransactionMonad o state (TransactionStatus e a)
begin f = withRollback (\abort -> do
            (stat, lbl) <- checkpoint
            when (isBegin stat)
              (f (TransactionHandle lbl) >> abort (Abort Nothing))
            return stat)

-- | Commit state, return a result.
commit   :: TransactionHandle o state e a ->       a -> TransactionMonad o state ()
commit    (TransactionHandle lbl) a = jump lbl (Commit a)

isBegin :: TransactionStatus e a -> Bool
isBegin Begin = True
isBegin _     = False

checkpoint :: TransactionMonad o s (TransactionStatus e a, Lbl o s (TransactionStatus e a))
checkpoint = withCommit (\commit ->
              withRollback (\rollback ->
  let go (Begin,      lbl) = error "TODO: nested transactions?"
      go (Abort e,    lbl) = rollback  (Abort e,     lbl)
      go (Commit a,   lbl) = commit    (Commit a,    lbl)
  in return (Begin, Lbl go)))

withCommit :: ((forall b. a -> TransactionMonad o s b) -> TransactionMonad o s a) -> TransactionMonad o s a
withCommit f = TransactionMonad (\s k -> unTxM (f (\a -> TransactionMonad (\s _ -> k s a))) s k)

withRollback :: ((forall b. a -> TransactionMonad o s b) -> TransactionMonad o s a) -> TransactionMonad o s a
withRollback f = TransactionMonad (\s k -> unTxM (f (\a -> TransactionMonad (\_ _ -> k s a))) s k)

jump :: Lbl o s a -> a -> TransactionMonad o s b
jump (Lbl k) a = k (a, Lbl k) >> undefined


runTxM_ :: TransactionMonad (a,s) s a -> s -> (a, s)
runTxM_ (TransactionMonad g) s = g s (\s a -> (a, s))

test0 :: TransactionHandle o Int String String -> TransactionMonad o Int ()
test0 tx = do
  s <- get
  set 99
  case s of
    _ -> commit tx "wooo"

-- `begin` type is `(TransactionHandle o state e a) -> (TransactionMonad o Int ())`
-- `runTxM_ type is `TransactionMonad (a,s) s a -> s -> (a, s)` , it applies the transaction state transformer
runTest0 :: [(TransactionStatus String String, Int)]
runTest0 = fmap (runTxM_ (begin test0)) [4] -- we lift `s -> (a, s)` over the array

spec :: Spec
spec = do
  describe "SimpleTransactionalState: example of a transactional state" $ do
      it "Commit the transaction" $ do
        runTest0 == [(Commit "wooo",99)]