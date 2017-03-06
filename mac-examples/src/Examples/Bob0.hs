module Examples.Bob0 where

import Data.Maybe
import Data.List

import qualified Examples.MACWget as Wget
import Data.ByteString.Lazy.Char8 (unpack)

import Data.List.Split

wget :: String -> a -> a -> IO String
wget url _ _ = unpack <$> Wget.wget url

-- Bob's code
common_pass :: String -> IO Bool
common_pass pass = fetchPassDict >>= return . isJust . find (==pass)


-- Reads passwords!
fetchPassDict :: IO [String]
fetchPassDict = do str <- wget "http://www.openwall.com/passwords/wordlists/password-2011.lst" [] []
                   let passwds = filter (not.null) (linesBy (=='\n') str)
                   return $ filter ( not . (=='#') . head ) passwds
