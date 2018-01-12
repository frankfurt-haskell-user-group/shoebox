{-# LANGUAGE OverloadedStrings #-}

module Shoebox.Util (
  readUtf8File,
  writeUtf8File,
  readLineConsole,
  writeLineConsole,
  showT,
  decodeFromText,
  encodeToText,
  decodeFromCbor,
  encodeToCbor,
  prettyTRNode,
  getCols
  )
where

import Data.ByteString.Base64 as B64
import Data.Text.Encoding
import Data.Binary.Serialise.CBOR as CBOR
import Data.Maybe
import Data.List (intersperse)

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as EN
import System.IO
import Data.Aeson
import Data.Aeson.Encode.Pretty

import qualified Data.HashMap.Lazy as M 
import qualified Data.Vector as V

import Shoebox.Data
import Shoebox.Translation
import Shoebox.Response as R

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
  hFlush stdout
  return ()

showT :: Show a => a -> T.Text
showT = T.pack . show

decodeFromText :: FromJSON a => T.Text -> Maybe a
decodeFromText = Data.Aeson.decode . BL.fromStrict . EN.encodeUtf8

encodeToText :: ToJSON a => a -> T.Text
encodeToText = EN.decodeUtf8 . BL.toStrict . encodePretty

decodeFromCbor :: Serialise a => T.Text -> Maybe a
decodeFromCbor base64 = let
  dr = (B64.decode . encodeUtf8) base64
  in case dr of
    Right d -> Just $ CBOR.deserialise (BL.fromStrict d) :: (Serialise a => Maybe a)
    Left s -> Nothing

encodeToCbor :: Serialise a => a -> T.Text
encodeToCbor msg = let
  e = BL.toStrict (CBOR.serialise msg)
  in (decodeUtf8 . B64.encode) e

prettyTRNode :: TRNode -> Value
prettyTRNode trn = let

  _pd d = case d of
                SbeText t -> String t
                SbeTextArray ta -> String $ T.concat $ intersperse ", " ta
                SbeNumber n -> Number (fromIntegral n)
  _pd' d = case d of
                SbeText t -> t
                SbeTextArray ta -> T.concat $ intersperse ", " ta
                SbeNumber n -> T.pack (show n)

  avl = Array . V.fromList
  avl'  = avl . map avl

  pl l = avl' (map (map _pd) (esFromTrl l))

  in case trn of
    TRN l [] -> Object (M.fromList [(_pd' (trlSource l), pl l)])
    TRN l ns -> Object (M.fromList [(_pd' (trlSource l), avl' (map (map prettyTRNode) ns))])

getCols :: Shoebox -> Value
getCols sb = let
  trs = sbTranslations sb
  ob tr = Object (M.fromList [("colName", String (sbdtMemo (trTargetTag tr))), ("lookups", toJSON (trLookup tr))])
  in ((Array . V.fromList) (map ob trs))
