{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Alice4 where

import MAC.Lattice
import MAC.Core hiding (MAC, try, runMAC)
import MAC.Labeled
import qualified Examples.MACWget as Wget
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Bits

import qualified Examples.Bob4 as Bob

type MAC m a = MACT m IO a

wgetMAC :: String -> MAC L String
wgetMAC = fmap unpack . Wget.wgetMAC

runMAC :: MAC l a -> IO a
runMAC = runMACT

{-
   Safe use of references. The password mananger uses memoization
   of Bob's function common_pass.
-}

password :: IO String
password = do wgetMem <- runMAC $ Bob.memMAC wgetMAC
              try wgetMem

try wget= do putStr "Please, select your password:"
             pass <- getLine
             lbool <- runMAC $ (label pass :: MAC L (Labeled H String))
                     >>= Bob.common_pass wget
             let MkId b = unRes lbool
             if b then putStrLn "Your password is too common!" >> (try wget)
               else return pass
