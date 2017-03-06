{-# LANGUAGE Trustworthy #-}

-- | Synchronization primitives
module MAC.MVar
    (
       MACMVar
     , newMACMVar
     , newMACEmptyMVar
     , takeMACMVar
     , putMACMVar
    )

where

import MAC.Lattice
-- Trustworthy
import MAC.MAC
-- Unsafe
import MAC.Effects

import Control.Concurrent.MVar

-- | Labeled @MVars@
type MACMVar l a = Res l (MVar a)

-- | Creation of a labeled @MVar@
newMACMVar :: Less l l' => a -> MACT l IO (MACMVar l' a)
newMACMVar = create . newMVar

-- | Creation of an empty labeled @MVar@
newMACEmptyMVar :: Less l l' => MACT l IO (MACMVar l' a)
newMACEmptyMVar = create newEmptyMVar

-- | Securely taking a labeled @MVar@
takeMACMVar :: Less l l => MACMVar l a -> MACT l IO a
takeMACMVar = rw_read takeMVar

-- | Securely writing into a labeled @MVar@
putMACMVar :: Less l l => MACMVar l a -> a -> MACT l IO ()
putMACMVar secmv v = rw_write (flip putMVar v) secmv
