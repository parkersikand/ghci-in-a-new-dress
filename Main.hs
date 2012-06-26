module Main where

import System.IO
import System.Process

import Yesod
import Yesod.Static

import GHCiYesod
import NewGHCi

main :: IO ()
main = do
    (Just hin, Just hout, Just herr, _) <-
      createProcess (proc ghciPath ghciArgs) {
              std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe
          }

    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering

    hPutStr hin ":t 1\n"
    _ <- hGetBlockInitial hout

    s <- staticDevel "static"

    warpDebug 3000 $ GHCiOnline s (hin, hout, herr)

