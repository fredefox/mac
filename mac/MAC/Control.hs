{-# LANGUAGE Trustworthy #-}

-- | Provide primitives to communicate among family members. It provides an
--   API for sequential 'joinMAC' and concurrent ('forkMAC') setting
module MAC.Control
    (
       joinMAC     -- Secure communication for sequential programs
     , forkMAC     -- Spawing threads
     , forkMACMVar -- Returning futures
    )

where

import MAC.Lattice
import MAC.Core (MACT, runMACT, Res (MkRes), lift, MonadCatch, MonadThrow, SomeException, catch, throwM)
import MAC.Labeled
import MAC.MVar

import Control.Concurrent
import Control.Monad

{-|
   Primitive which allows family members to safely communicate. The function
   finishes even if an exception is raised---the exception is rethrown when
   the returned value gets inspected.
   __This function must not be used in a concurrent setting__.
-}

-- `MonadCatch` provides `throwM :: (Exception e, MonadThrow m) => e -> m a` but
-- this is not directly applicable here. For this reason the type has changed
-- from this:
--
--     joinMAC :: (Less l l', MonadCatch m) => MACT l' m a -> MACT l m (Labeled l' a)

-- joinMAC   :: (Less l l', MonadCatch m) => MACT l' m a -> MACT l m (Labeled l' (m a))
-- joinMAC m = (lift . runMACT)
--               (catch (m >>= safe_label) hd)
--               where safe_label = return . MkRes . MkId
--                     hd = safe_label . throwM . proxy
--                     proxy :: SomeException -> SomeException
--                     proxy = id

joinMAC :: (Less l l', MonadCatch m) => MACT l' m a -> MACT l m (Labeled l' a)
joinMAC m = liftStuff
  where
    -- liftStuff :: MACT l m (Labeled l' a)
    liftStuff = lift . runMACT $ catchStuff
    -- catchStuff :: MACT l' m (Labeled l' a)
    catchStuff = catch (safe_label <$> m) hd
    -- safe_label :: a -> Labeled l' a
    safe_label = MkRes . MkId
    hd :: MonadThrow m => SomeException -> MACT l' m (Labeled l' a)
    hd = fmap safe_label . throwM


{-
  Note:

  Instead of safe_label, it is possible to use the primitive label. In that
  manner, we do not break abstraction and we have more confidence about the
  correctness of the implementation. However, by doing so, the type signature
  needs to add Less l' l' into the type constraints.  Since that constraint
  always hold, it can be show that m >>= label and label (throw e) is equivalent
  to m >>= safe_label and safe_label (throw e) in joinMAC.
-}



-- TODO: Why would you not return the ThreadID?
-- | Safely spawning new threads
forkMAC :: Less l l' => MACT l' IO () -> MACT l IO ()
forkMAC m = void $ lift . forkIO . runMACT $ m

{-|
   Safely spawning new threads. The function returns a labeled 'MVar' where
   the outcome of the thread is stored
-}
forkMACMVar :: (Less l' l', Less l l') => MACT l' IO a -> MACT l IO (MACMVar l' a)
forkMACMVar m = do lmv <- newMACEmptyMVar
                   forkMAC (m >>= putMACMVar lmv)
                   return lmv
