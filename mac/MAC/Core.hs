{-# LANGUAGE Safe #-}

-- | It defines main data structures for security, i.e., monad family 'MAC' and
--   labeled resources 'Res'.
module MAC.Core
    (
     -- Resources definitions
       Res (MkRes,unRes)
     , mkRes
     , labelOf
     -- Monad MAC
     , MACT (MACT)
     , runMACT
     , MAC
     , runMAC
     -- TODO: If we expose this type-class then an attacker can just make a
     -- derived instance for non-safe computations.
     , Public()
     -- Do we really want to re-export all these modules - I think not?
     , module Control.Monad.IO.Class
     , module Control.Monad.Trans.Class
     , module Control.Monad.Catch
    )

where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Identity

-- | Labeling expressions of type @a@ with label @l@.
newtype Res l a = MkRes {unRes :: a}

mkRes :: a -> Res l a
mkRes = MkRes

instance Functor (Res l) where
  fmap f (MkRes a) = MkRes (f a)

-- | Label of resources
labelOf :: Res l a -> l
labelOf _res = undefined

newtype MACT l m a = MACT
  {
  -- | Execute secure computations.
  _runMACT :: IdentityT m a
  }

runMACT :: MACT l f a -> f a
runMACT = runIdentityT . _runMACT

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
   MACT m >>= k = MACT (m >>= _runMACT . k)

-- TODO: Are we really forced to add the constraint `Public l` just like for MonadIO?
--
-- Note that `lift` can be used to do the same as `liftIO`:
--
--     runMACT $ (lift getLine :: MACT H IO String)
instance MonadTrans (MACT l) where
  lift = MACT . lift

-- The original function `ioTCB` had a comment that it should not be exported.
-- This will prove tricky if we make MACT an instance of MonadIO.
-- I think we can get around this by requiring that whenever we lift an IO-computation
-- into this monad we end up with a "public" computation. So we need a type-class
-- to encapsulate this. I think we'll need a similar restriction on `MonadTrans`
-- sine we can also embed io-actions using that typeclass.
instance (Public l, MonadIO m) => MonadIO (MACT l m) where
  liftIO = lift . liftIO

{-|
   Throwing exceptions
-}
instance (MonadThrow m) => MonadThrow (MACT l m) where
  throwM = lift . throwM

{-|
   Throwing and catching exceptions are done among family members with the
   same labels
-}
instance (MonadCatch m) => MonadCatch (MACT l m) where
  catch (MACT io) hd = MACT $ catch io (_runMACT . hd)

class Public a where
