module Main where

import Data.Maybe
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
    hPutStr hin "import GHC.GHCi\n"
    hPutStr hin ":runmonad NoIO\n"
    intro <- hGetBlockInitial hout
    putStrLn intro 

    s <- staticDevel "static"

    warpDebug 3000 $ GHCiOnline s (hin, hout, herr)

