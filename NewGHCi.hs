-- | GHCi related functions
module NewGHCi where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.List (isInfixOf)
import System.IO (Handle, hGetChar, hGetLine, hPutStr, hReady)
import System.IO.Unsafe (unsafePerformIO)

import Utils

-- TODO: locks are bad MKay!
lockGHCI :: MVar Bool
{-# NOINLINE lockGHCI #-}
lockGHCI = unsafePerformIO (newMVar True)

ghciPath :: FilePath
ghciPath = "/home/davidt/Ghc/ghci-pkg/dist/build/ghci-safe/ghci-safe"

ghciArgs :: [String]
ghciArgs = []

sentinel :: String
sentinel = "1234567890"

queryGHCI :: (Handle, Handle, Handle) -> String -> IO String
queryGHCI h input | last input /= '\n' = queryGHCI h $ input ++ "\n"

queryGHCI (hin, hout, herr) input = do
    -- Lock this function. Only 1 person can query ghci at a time.
    _ <- takeMVar lockGHCI
  
    hPutStr hin input

    errors <- do
        hPutStr hin "oopsthisisnotavariable\n"
        err <- getErrors herr
        return err
  
    -- This is a hack that lets us discover where the end of the output is.
    -- We will keep reading until we see the sentinel.
    hPutStr hin (":t " ++ sentinel ++ "\n")

    output <- readUntilDone hout

    putMVar lockGHCI True

    if trimWhitespace errors == "" 
        then return output
        else return $ "ERR: " ++ show (parseErrors errors)

getErrors :: Handle -> IO String
getErrors herr' = 
    go herr' ""
  where
    go herr results = do
      line <- hGetLine herr
      putStrLn $ "Error: " ++ line

      if "oopsthisisnotavariable" `isInfixOf` line
        then return(results)
        else go herr (results ++ "\n" ++ line)

readUntilDone :: Handle -> IO String
readUntilDone hout = go []
  where
    go acc = do
      l <- hGetLine hout
      putStrLn $ "Input: " ++ l
      if sentinel `isInfixOf` l
        then return $ done acc
        else go (l:acc)
    
    done [] = "\n"
    done xs = unlines $ reverse xs

hGetBlockInitial :: Handle -> IO String
hGetBlockInitial h = do
    l <- hGetLine h
    putStrLn $ "Input: " ++ l
    ls <- hGetAvailable h
    if null ls
        then return l
        else return $ l ++ "\n" ++ ls

hGetAvailable :: Handle -> IO String
hGetAvailable h = go ""
  where
    go acc = do
        r <- hReady h
        case r of
            True  -> hGetChar h >>= \c -> go (c:acc)
            False -> return $ reverse acc

