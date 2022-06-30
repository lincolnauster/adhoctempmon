{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Monitor
  ( TempSource
  , readTemps
  ) where

import Control.Concurrent
import Control.Exception

import Data.Functor

import System.Directory
import System.IO
import System.IO.Error
import System.Posix

import Util

type TempSource = (Maybe String, Handle)

fifoPath :: FilePath
fifoPath = "/tmp/tempmon.fifo"

readDelay :: Int
readDelay = 1000000

readTemps :: (Maybe String -> IO ()) -> MVar () -> IO ()
readTemps func term = getSource >>= forTemp func term >>= fifoCleanup

getSource :: IO TempSource
getSource =
  (getFifo >>= ensureFifo <&> (Nothing, )) <* logEv "Obtained source FIFO."
  where
    getFifo = catch (openFile fifoPath ReadMode) createForUse
    ensureFifo f =
      handleToFd f >>= getFdStatus >>= \s ->
        if isNamedPipe s
          -- The call to isNamedPipe can close the file, so let's reopen.
          then openFile fifoPath ReadMode
          else ioError $
               mkIOError
                 illegalOperationErrorType
                 "Not a FIFO"
                 (Just f)
                 (Just fifoPath)
    createForUse e =
      if isDoesNotExistError e
        then (createNamedPipe fifoPath 0o666 >> openFile fifoPath ReadMode) <*
             logEv "Created source FIFO."
        else ioError e

-- If data is available, read and return it. Otherwise, keep the
-- old value.
-- 
-- TODO: Return a meaningful reading, not a String.
readTemp :: TempSource -> IO TempSource
readTemp (curr, hand) =
  let v =
        hIsEOF hand >>= \b ->
          if b
            then logEv "Using cached temperature." $> curr
            else logEv "Reading new temperature." *> hGetLine hand <&> Just
   in v <&> (, hand)

forTemp :: (Maybe String -> IO ()) -> MVar () -> TempSource -> IO TempSource
forTemp f term as =
  tryTakeMVar term >>= \case
    Nothing ->
      readTemp as >>= \(t, s) ->
        f t >> threadDelay readDelay >> forTemp f term (t, s)
    Just () -> return as

fifoCleanup :: TempSource -> IO ()
fifoCleanup (_, h) = hClose h >> removeFile fifoPath <* logEv "Cleaned up FIFO."
