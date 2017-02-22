{-# LANGUAGE Trustworthy #-}

-- | Mutuable state (references)
module MAC.Ref
    (
       MACRef
     , newMACRef
     , readMACRef
     , writeMACRef
    )

where

import MAC.Lattice
import Data.IORef

-- Trustworthy
import MAC.MAC
-- Unsafe
import MAC.Effects

-- | Labeled references
type MACRef l a = Res l (IORef a)

-- | Creation of labeled references
newMACRef :: Less l l' => a -> MACT l IO (MACRef l' a)
newMACRef = create . newIORef

-- | Reading labeled references
readMACRef :: Less l' l => MACRef l' a -> MACT l IO a
readMACRef = readdown readIORef

-- | Writing labeled references
writeMACRef :: Less l l' => MACRef l' a -> a -> MACT l IO ()
writeMACRef secref v = writeup (flip writeIORef v) secref
