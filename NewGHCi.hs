-- | GHCi related functions
module NewGHCi where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.List (isInfixOf, isPrefixOf)
import System.IO (Handle, hGetChar, hGetLine, hPutStr, hReady)
import System.IO.Unsafe (unsafePerformIO)

import Tools
import Utils

-- TODO: locks are bad MKay!
lockGHCI :: MVar Bool
{-# NOINLINE lockGHCI #-}
lockGHCI = unsafePerformIO (newMVar True)

ghciPath :: FilePath
ghciPath = "/home/davidt/Software/ghc-ghci/bin/ghci"

ghciArgs :: [String]
ghciArgs = ["-XSafe", "-fpackage-trust", "-distrust-all-packages", "-trust base"]

sentinel :: String
sentinel = "1234567890"

queryGHCI :: (Handle, Handle, Handle) -> String -> IO String
queryGHCI h input | last input /= '\n'           = queryGHCI h $ input ++ "\n"
queryGHCI _ input | ":doc" `isPrefixOf` input    = queryHaddock input
queryGHCI _ input | ":hoogle" `isPrefixOf` input = queryHoogle input
queryGHCI (hin, hout, herr) input = do
    -- Lock this function. Only 1 person can query ghci at a time.
    _ <- takeMVar lockGHCI

    -- Get Hlint suggests only if it's Haskell code and not an interpretor
    -- command If "No suggestions", then don't send it in down and if it already
    -- ends with '\n', don't do anything
    hlint <- if ":" `isPrefixOf` input
              then return ""
              else hlintCheck input
    let hlintSugg = if ":" `isPrefixOf` input
                      then hlint
                      else hlint ++ "\n"
  
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
        then return $ hlintSugg ++ output
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

