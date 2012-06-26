{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module GHCiYesod where

import qualified Data.Text as T (unpack)
import System.IO

import Yesod
import Yesod.Static

import NewGHCi
import Utils

staticFiles "static"

mkYesod "GHCiOnline" [parseRoutes|
/       HomeR   GET
/ghci   GHCIR   POST
/static StaticR Static helloWorldStatic
|]

data GHCiOnline = GHCiOnline {
    helloWorldStatic :: Static,
    handles :: (Handle, Handle, Handle)
  }

instance Yesod GHCiOnline where
    approot = ApprootRelative

postGHCIR :: Handler RepHtml
postGHCIR = do
  -- This is how you get post data. 
  -- type of postTuples is [(Data.Text, Data.Text)] - key value pairs
  (postTuples, _) <- runRequestBody
  let content = unescape $ T.unpack (snd $ postTuples !! 0)
  y <- getYesod
  result <- liftIO $ queryGHCI (handles y) content
  defaultLayout [whamlet|#{result}|]

getHomeR :: Handler RepHtml
getHomeR = do
  y <- getYesod
  _ <- liftIO $ queryGHCI (handles y) ":t 5.0\n"
  defaultLayout [whamlet|
<html>
  <head>
    <title> GHCi Online (Powered by Safe Haskell)
    <link rel="stylesheet" type="text/css" href="static/style.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js">
    <script src="static/main.js">
  <body>
    <ul id="autocomplete"> 
    <div id="typeannotations">
    <div id="calltips">
    <div id="program">
      <div id="console">
        <div class="input" id="active">
          <span id="prompt">
            \$ 
          <span id="content">
          <span id="cursor">_
        <div id="sidebar">
          <h3>Inspector
          <ul id="sidelist">
|]

