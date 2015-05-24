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
main = do
  print "Inside main"
  suffixMap <- suffixFrequencies
  quickHttpServe (site suffixMap)

site :: SuffixFrequencyMap -> Snap ()
site suffixMap =
    ifTop (writeText1 suffixMap) <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")


writeText1 :: SuffixFrequencyMap -> Snap ()
writeText1 suffixMap = do
  text <- liftIO $ (getText suffixMap)
  writeBS text
  return ()

getText :: SuffixFrequencyMap -> IO ByteString
getText suffixMap = do
  text <- generateText suffixMap 300
  return $ Data.ByteString.UTF8.fromString text

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
