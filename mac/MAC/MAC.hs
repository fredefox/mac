{-# LANGUAGE Trustworthy #-}

-- | A safe interface for module @Core.hs@
module MAC.MAC
    (
     -- It comes from Core
       Res ()
     , mkRes
     , labelOf
     -- Monad MAC
     , MACT ()
     , runMACT
     , MAC ()
     , runMAC
     -- Auxiliary proxies
     , fix
    )

where

import MAC.Core

-- | To help the type-system
fix :: Monad m => l -> MACT l m ()
fix _l = return ()
