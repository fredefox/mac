{-# LANGUAGE Trustworthy #-}

-- | Exceptions
module MAC.Exception
    (
       throwMAC
     , catchMAC
    )

where

import MAC.Core (MACT(MACT), runMACT, lift)
import Control.Exception (Exception, catch, throw)

{-|
   Throwing exceptions
-}
throwMAC :: (Monad m, Exception e) => e -> MACT l m a
throwMAC = lift . throw


{-|
   Throwing and catching exceptions are done among family members with the
   same labels
-}
catchMAC :: Exception e => MACT l IO a -> (e -> MACT l IO a) -> MACT l IO a
catchMAC (MACT io) hd = lift $ catch io (runMACT . hd)
