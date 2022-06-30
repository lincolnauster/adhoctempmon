{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad.IO.Class

import Data.Functor

import System.IO.Error ()

import System.Posix.Signals

import Web.Scotty

import Monitor (readTemps)
import Pages

-- Create and return an MVar which is set on SIGINT.
catchSigint :: IO (MVar ())
catchSigint =
  newEmptyMVar >>= \mv ->
    installHandler sigINT (CatchOnce $ putMVar mv ()) Nothing $> mv

report :: IOError -> IO ()
report e = putStrLn $ "\ESC[1;31m" ++ show e ++ "\ESC[0m"

monitor :: (Maybe String -> IO ()) -> IO ()
monitor cb = catchSigint >>= mainloop
  where
    mainloop mv = catch (readTemps cb mv) report

homepage :: (Show a) => TVar (Maybe a) -> ScottyM ()
homepage t = get "/" $ liftIO (readTVarIO t <&> renderIndex) >>= html

stylesheet :: ScottyM ()
stylesheet = get "/static/main.css" mainCss
  where
    mainCss =
      setHeader "Content-Type" "text/css; charset=utf-8" >>
      file "static/main.css"

-- Main orchestrates the web server and its communication with the
-- temperature monitor. Communication is done through a TVar updated
-- in the monitor's update callback. The program terminates with the
-- original thread, and, as Scotty's thread can't be terminated in any
-- other way, we run the monitor subroutine in the original thread so
-- that killing the monitor on process SIGINT brings the whole thing
-- down cleanly.
main :: IO ()
main =
  newTVarIO Nothing >>= \temp -> forkIO (serve temp) >> monitor (update temp)
  where
    update temp val = atomically $ writeTVar temp val
    serve temp = scotty 8080 $ stylesheet >> homepage temp
