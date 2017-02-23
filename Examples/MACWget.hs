{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.MACWget where

import MAC.Lattice
import MAC.Core (MACT, lift)

import Control.Monad.IO.Class
import Network.HTTP.Simple
import Data.ByteString.Lazy (ByteString)

{-
  For simplicity, when wgetMAC gets called with http://bob.evil as a domain, it
  will write the request to a file
-}
wgetMAC :: String -> MACT L IO ByteString
wgetMAC s | take (length domain) s == domain =
              lift (appendFile "leaks.txt" (s ++ "\n")) >> return "launch"
          | otherwise = wget s
          where domain = "http://bob.evil"

wget :: MonadIO m => String -> m ByteString
wget = fmap getResponseBody . httpLBS . parseRequest_
