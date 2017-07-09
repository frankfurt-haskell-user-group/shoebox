{-# LANGUAGE OverloadedStrings #-}

module Shoebox.Util (
  readUtf8File,
  writeUtf8File,
  readLineConsole,
  writeLineConsole,
  showT,
  decodeFromText,
  encodeToText
  )
where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as EN
import System.IO
import Data.Aeson

_cleanCR =  T.replace (T.pack "\r") (T.pack "\n") . T.replace (T.pack "\r\n") (T.pack "\n")  

readUtf8File :: T.Text -> IO T.Text
readUtf8File fileName = do
  dbTxt <- ( _cleanCR  . EN.decodeUtf8) <$> BS.readFile (T.unpack fileName)
  return dbTxt

writeUtf8File :: T.Text -> T.Text -> IO ()
writeUtf8File fileName t = do
  BS.writeFile (T.unpack fileName) (EN.encodeUtf8 t)
  return ()


readLineConsole :: IO T.Text
readLineConsole = do
  dbTxt <- (_cleanCR . EN.decodeUtf8) <$> BS.hGetLine stdin
  return (T.strip dbTxt)

writeLineConsole :: T.Text -> IO ()
writeLineConsole t = do
  BS.hPut stdout (EN.encodeUtf8 t)
  BS.hPut stdout "\n"
  return ()

showT :: Show a => a -> T.Text
showT = T.pack . show

decodeFromText :: FromJSON a => T.Text -> Maybe a
decodeFromText = decode . BL.fromStrict . EN.encodeUtf8

encodeToText :: ToJSON a => a -> T.Text
encodeToText = EN.decodeUtf8 . BL.toStrict . encode 