{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Alice3 where

import MAC.Lattice
import MAC.Core hiding (MAC, runMAC)
import MAC.Labeled

import Data.Bits
import qualified Examples.Bob3 as Bob

type MAC m a = MACT m IO a

runMAC :: MAC l a -> IO a
runMAC = runMACT

{-
   Bob's code tries to exploit leaking information by leveraging
   exceptions and joinMAC.
-}

password :: IO String
password = do putStr "Please, select your password:"
              pass <- getLine
              lbool <- runMAC $ (label pass :: MAC L (Labeled H String))
                      >>= Bob.common_pass
              let MkId b = unRes lbool
              if b
                 then putStrLn "Your password is too common!" >> password
                 else return pass
