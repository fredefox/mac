{-# LANGUAGE Unsafe #-}

-- | It defines main data structures for security, i.e., monad family 'MAC' and
--   labeled resources 'Res'.
module MAC.Core
    (
     -- Resources definitions
       Res (MkRes,unRes)
     , labelOf
     -- Monad MAC
     , MACT (MACT)
     , runMACT
     , MAC
     , runMAC
     , module Control.Monad.Trans.Class
    )

where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- | Labeling expressions of type @a@ with label @l@.
newtype Res l a = MkRes {unRes :: a}

-- | Label of resources
labelOf :: Res l a -> l
labelOf _res = undefined

newtype MACT l m a = MACT
  -- | Execute secure computations.
  { runMACT :: m a
  }

{-|
    This monad labels the results of the computation (of type @a@) with
    label @l@.
-}
type MAC l = MACT l Identity

runMAC :: MAC l c -> c
runMAC = runIdentity . runMACT

instance Monad m => Functor (MACT l m) where
    fmap f (MACT m) = MACT (fmap f m)

instance Monad m => Applicative (MACT l m) where
    pure = MACT . return
    (<*>) (MACT f) (MACT a) = MACT (f <*> a)

instance Monad m => Monad (MACT l m) where
   return = pure
   MACT m >>= k = lift (m >>= runMACT . k)

instance MonadTrans (MACT l) where
  lift = MACT

-- The original function `ioTCB` had a comment that it should not be exported.
-- This will prove tricky if we make MACT an instance of MonadIO.
instance MonadIO m => MonadIO (MACT l m) where
  liftIO = lift . liftIO
