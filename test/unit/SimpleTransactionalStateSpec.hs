{-# LANGUAGE RankNTypes #-}

module SimpleTransactionalStateSpec where

import Control.Monad (when)

import Test.Hspec

-- | Transaction Status.
data TxStat e a
  = Begin
  | Abort (Maybe e)       -- ^ Reverted  state, returned an error.
  | Commit a              -- ^ Committed state, returned a result.
  deriving(Eq,Ord,Read,Show)

-- | A transaction handle.
newtype Tx o s e a  = Tx  {unTx :: Lbl o s (TxStat e a)}

-- | The transaction monad. A State monad, with transactional state.
newtype TxM o s a   = TxM {unTxM :: s -> (s -> a -> o) -> o}

instance Applicative (TxM o s) where
    pure = return
    fs <*> s = do f <- fs
                  v <- s
                  return (f v)
instance Functor (TxM o s) where
  fmap f (TxM g) = TxM (\s k -> g s (\s a -> k s (f a)))
instance Monad (TxM o s) where
  return a = TxM (\s k -> k s a)
  TxM g >>= f = TxM (\s k -> g s (\s a -> unTxM (f a) s k))

newtype Lbl o s a   = Lbl {unLbl :: (a, Lbl o s a) -> TxM o s ()}

set :: s -> TxM o s ()
set s = TxM (\_ k -> k s ())

get :: TxM o s s
get = TxM (\s k -> k s s)

-- | Begin a transaction. @begin@ takes a function
--  which represents this transaction.
begin :: (Tx o s e a -> TxM o s ()) -> TxM o s (TxStat e a)
begin f = withRollback (\abort -> do
            (stat, lbl) <- checkpoint
            when (isBegin stat)
              (f (Tx lbl) >> abort (Abort Nothing))
            return stat)

-- | Commit state, return a result.
commit   :: Tx o s e a ->       a -> TxM o s ()
commit    (Tx lbl) a = jump lbl (Commit a)

isBegin :: TxStat e a -> Bool
isBegin Begin = True
isBegin _     = False

checkpoint :: TxM o s (TxStat e a, Lbl o s (TxStat e a))
checkpoint = withCommit (\commit ->
              withRollback (\rollback ->
  let go (Begin,      lbl) = error "TODO: nested transactions?"
      go (Abort e,    lbl) = rollback  (Abort e,     lbl)
      go (Commit a,   lbl) = commit    (Commit a,    lbl)
  in return (Begin, Lbl go)))

withCommit :: ((forall b. a -> TxM o s b) -> TxM o s a) -> TxM o s a
withCommit f = TxM (\s k -> unTxM (f (\a -> TxM (\s _ -> k s a))) s k)

withRollback :: ((forall b. a -> TxM o s b) -> TxM o s a) -> TxM o s a
withRollback f = TxM (\s k -> unTxM (f (\a -> TxM (\_ _ -> k s a))) s k)

jump :: Lbl o s a -> a -> TxM o s b
jump (Lbl k) a = k (a, Lbl k) >> undefined


runTxM_ :: TxM (a,s) s a -> s -> (a, s)
runTxM_ (TxM g) s = g s (\s a -> (a, s))

test0 :: Tx o Int String String -> TxM o Int ()
test0 tx = do
  s <- get
  set 99
  case s of
    _ -> commit   tx  "wooo"

runTest0 :: [(TxStat String String, Int)]
runTest0 = fmap (runTxM_ (begin test0)) [4]

spec :: Spec
spec = do
  describe "SimpleTransactionalState: example of a transactional state" $ do
      it "Commit the transaction" $ do
        runTest0 == [(Commit "wooo",99)]