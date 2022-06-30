module Util
  ( logEv
  ) where

import System.IO
import Data.Time

logEv :: String -> IO ()
logEv s =
  time >>= (\t -> return $ "\ESC[1;33m" ++ t ++ "\ESC[0m: " ++ s) >>=
  hPutStrLn stderr
  where
    time = show <$> getZonedTime

