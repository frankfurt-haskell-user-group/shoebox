{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric  #-}
module Shoebox.TranslationRules where

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

data TranslationRule = TRMorphemeBreak SBDataTag -- database contains morpheme breaks at tag, source will be splitted in mulitple parts
               | TRKey -- keys of database contains the tags, we simply use as is
               | TRTranslate SBDataTag -- target tag contains translation of key tag
               deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TranslationRule
instance FromJSON TranslationRule

data Translation = Translation {
  trTextDB :: SBDbIdent,
  trSourceTag :: SBDataTag,
  trTargetTag :: SBDataTag,
  trRules :: [(TranslationRule, SBDbIdent)]
} deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Translation
instance FromJSON Translation

-- |The data plus a set of query rules defines the shoebox
data Shoebox = Shoebox {
    sbData :: ShoeboxData,
    sbTranslations  :: [Translation] 
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Shoebox
instance FromJSON Shoebox

newShoebox :: Shoebox
newShoebox = let
    sbData = newShoeboxData
    sbTranslations = [
      Translation textDbId txTag mbTag [
        (TRMorphemeBreak bkTag, parsingDbId),
        (TRKey, lexDbId),
        (TRKey, suffixDbId) 
        ]
      , Translation textDbId mbTag glTag [
        (TRTranslate meTag, lexDbId),
        (TRTranslate meTag, suffixDbId)
        ]
      ]
    in Shoebox sbData sbTranslations


translationLength :: Shoebox -> Int
translationLength sb = length (sbTranslations sb)

data TRNode = TRN {
    trnData :: Either SBError (Maybe SBDataEntry),
    trnResult :: [TRNode]
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TRNode
instance FromJSON TRNode

translateRule :: Shoebox -> (TranslationRule, SBDbIdent) -> SBDataEntry -> Either SBError (Maybe SBDataEntry)
translateRule sb (r, si) de = let
  (ShoeboxData dmap) = (sbData sb)
  db = fromJust $ M.lookup si dmap
  in case r of
    TRMorphemeBreak tag -> dbQuery db de tag
    TRKey -> dbQueryKey db de
    TRTranslate tag -> dbQuery db de tag

translate :: Shoebox -> Translation -> TRNode -> TRNode
translate sb q trn = let
  e = trnData trn
  in case e of
    Right (Just e') -> let
      r = foldl (\a b -> case a of
        Right Nothing -> translateRule sb b e'
        _ -> a
        ) (Right Nothing) (trRules q)
      c = case r of
            Right (Just (SbeTextArray ta)) ->  fmap (\t -> TRN (Right (Just (SbeText t))) []) ta
            _ -> [TRN r []]
      in trn { trnResult = c }
    _ -> trn

translateEntry :: Shoebox -> TRNode -> TRNode
translateEntry sb trn =
  let _translate trns trn = case trns of
        [] -> trn
        (t:ts) -> let
          trn' = translate sb t trn
          in TRN (trnData trn') (map (_translate ts) (trnResult trn'))
  in _translate (sbTranslations sb) trn

