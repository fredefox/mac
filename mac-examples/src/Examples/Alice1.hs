{-# LANGUAGE Trustworthy #-}
module Examples.Alice1 where

import MAC.Lattice
import MAC.Core hiding (MAC, runMAC)
import MAC.Labeled

import qualified Examples.Bob1 as Bob

type MAC m a = MACT m IO a

runMAC :: MAC l a -> IO a
runMAC = runMACT


{-
  In this example, Bob code is malicious. It utilizes unsafePerformIO to fool
  the security restrictions imposed by the security types. This problem can be
  fixed by using Safe Haskell --- just add {-# LANGUAGE Safe #-} to the
  file Bob1.hs
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
