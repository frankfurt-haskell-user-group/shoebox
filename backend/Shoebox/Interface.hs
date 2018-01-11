{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Shoebox.Interface 

where

import Text.Printf
import Data.List (intercalate)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe

import System.Directory (getDirectoryContents, removeFile)
import System.FilePath (dropExtension, takeBaseName)
import Data.List.Split (splitOn)
import qualified Data.Map as M

import qualified System.IO.Strict as S

import Data.Typeable
import Data.Data
import Data.Aeson
import GHC.Generics

import Shoebox.Data
import Shoebox.Translation
import Shoebox.Parser
import Shoebox.Util

import qualified Data.Set as S
import qualified Data.Text as T


loadDB :: T.Text -> SBDatabase -> IO (Either SBError SBDatabase)
loadDB file db = do
  dbTxt <- readUtf8File file 
  case parseDB dbTxt db of
    Left err -> return $ Left err
    Right records -> return $ Right (dbInsertFromRecords db records)

listDBs :: T.Text -> IO [Text]
listDBs dirname = do
    files <- getDirectoryContents (T.unpack dirname)
    let f' = map (T.replace ".sbx" "" . T.replace ".u8" "" . T.replace "sf.u8" "" . T.replace "ps.u8" "" . T.pack) files
    let f'' = ( filter (\n -> not (n `elem` ["..", "."])) . S.toList . S.fromList ) f'
    return f''

saveShoebox :: Shoebox -> T.Text -> T.Text -> IO ()
saveShoebox sb dirName dbName = do
    let fname = T.concat [dirName, "/", dbName, ".sbx"]
    writeUtf8File fname (encodeToText sb)
    return ()

readShoebox :: T.Text -> T.Text -> IO Shoebox
readShoebox dirName dbName = do
    -- check new file
    files <- getDirectoryContents (T.unpack dirName)  
    if (T.unpack dbName) ++ ".sbx" `elem` files
      then do
        let fname = T.concat [dirName, "/", dbName, ".sbx"]
        d <- readUtf8File fname
        let (Just sb) = decodeFromText d
        return sb 
      else readOldDBs $ T.concat [dirName, "/", dbName]


readOldDBs :: T.Text -> IO Shoebox
readOldDBs fname = do
    let fromRight a = let (Right a') = a in a'
    let sb = newShoebox
    lDB <- fromRight <$> loadDB (T.concat [fname, ".u8"]) lexDB 
    pDB <- fromRight <$> loadDB (T.concat [fname, "ps.u8"]) parsingDB 
    sDB <- fromRight <$> loadDB (T.concat [fname, "sf.u8"]) suffixDB 
    let sbd = ShoeboxData $ M.fromList [
                (textDbId, textDB),
                (parsingDbId, pDB),
                (lexDbId, lDB),
                (suffixDbId, sDB)
                ]
    return $ sb {sbData = sbd}

deleteDB :: Text -> Text -> IO ()
deleteDB dirName dbName = do
    let fname = T.concat [dirName, "/", dbName, ".sbx"]
    removeFile (T.unpack fname)

