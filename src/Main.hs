{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           TextGeneration
import           Data.ByteString
import           Data.ByteString.UTF8
import           Control.Monad.IO.Class

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeText1) <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")


writeText1 :: Snap ()
writeText1 = do
  text <- liftIO $ getText
  writeBS text
  return ()

getText :: IO ByteString
getText = do
  suffixMap <- suffixFrequencies
  text <- generateText suffixMap 300
  return $ Data.ByteString.UTF8.fromString text

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
