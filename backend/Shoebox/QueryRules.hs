{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric  #-}
module Shoebox.QueryRules where

import Text.Printf
import Data.List (intercalate)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe

import Data.Data
import Data.Typeable
import GHC.Generics
import Data.Aeson

import Shoebox.Data

data QRFlag = ParsingFlag -- fields found with no available data-flag are taken as-is, if target found in bk, line is broken up
              deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON QRFlag
instance FromJSON QRFlag

data QueryRule = QRMorphemeBreak SBDataTag -- database contains morpheme breaks at tag, source will be splitted in mulitple parts
               | QRKey -- keys of database contains the tags, we simply use as is
               | QRTranslate SBDataTag -- target tag contains translation of key tag
               deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON QueryRule
instance FromJSON QueryRule

data Query = Query {
  qrTextDB :: SBDbIdent,
  qrSourceTag :: SBDataTag,
  qrTargetTag :: SBDataTag,
  qrRules :: [(QueryRule, SBDbIdent)]
} deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Query
instance FromJSON Query

-- |The data plus a set of query rules defines the shoebox
data Shoebox = Shoebox {
    sbData :: ShoeboxData,
    sbQueries  :: [Query] 
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Shoebox
instance FromJSON Shoebox

newShoebox :: Shoebox
newShoebox = let
    sbData = newShoeboxData
    sbQueries = [
      Query textDbId txTag mbTag [
        (QRMorphemeBreak bkTag, parsingDbId),
        (QRKey, lexDbId),
        (QRKey, suffixDbId) 
        ]
      , Query textDbId mbTag glTag [
        (QRTranslate meTag, lexDbId),
        (QRTranslate meTag, suffixDbId)
        ]
      ]
    in Shoebox sbData sbQueries


queryLength :: Shoebox -> Int
queryLength sb = length (sbQueries sb)    

data QueryNode = QN {
    qnData :: Either SBError (Maybe SBDataEntry),
    qnResult :: [QueryNode]
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON QueryNode
instance FromJSON QueryNode

queryRule :: Shoebox -> (QueryRule, SBDbIdent) -> SBDataEntry -> Either SBError (Maybe SBDataEntry)
queryRule sb (r, si) de = let
  (ShoeboxData dmap) = (sbData sb)
  db = fromJust $ M.lookup si dmap
  in case r of
    QRMorphemeBreak tag -> dbQuery db de tag
    QRKey -> dbQueryKey db de
    QRTranslate tag -> dbQuery db de tag

queryQuery :: Shoebox -> Query -> QueryNode -> QueryNode
queryQuery sb q qn = let
  e = qnData qn
  in case e of
    Right (Just e') -> let
      r = foldl (\a b -> case a of
        Right Nothing -> queryRule sb b e'
        _ -> a
        ) (Right Nothing) (qrRules q)
      c = case r of
            Right (Just (SbeTextArray ta)) ->  fmap (\t -> QN (Right (Just (SbeText t))) []) ta
            _ -> [QN r []]
      in qn { qnResult = c }
    _ -> qn

queryEntry :: Shoebox -> QueryNode -> QueryNode
queryEntry sb qnode = 
  let _query queries qn = case queries of
        [] -> qn
        (q:qs) -> let 
          qn' = queryQuery sb q qn 
          in QN (qnData qn') (map (_query qs) (qnResult qn'))
  in _query (sbQueries sb) qnode

