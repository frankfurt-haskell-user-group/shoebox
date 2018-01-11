{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric  #-}
module Shoebox.Translation where

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

data LookupOption = LUMorphemeBreak SBDbIdent SBDataTag -- database contains morpheme breaks at tag, source will be splitted in mulitple parts
               | LUKey SBDbIdent -- keys of database contains the tags, we simply use as is
               | LUTranslate SBDbIdent SBDataTag -- target tag contains translation of key tag
               deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON LookupOption
instance FromJSON LookupOption

-- each translation transforms from source to target, with multiple lookup options
data Translation = Translation {
  trTextDB :: SBDbIdent,
  trSourceTag :: SBDataTag,
  trTargetTag :: SBDataTag,
  trLookup :: [LookupOption]
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
        (LUMorphemeBreak parsingDbId bkTag),
        (LUKey lexDbId),
        (LUKey suffixDbId) 
        ]
      , Translation textDbId mbTag glTag [
        (LUTranslate lexDbId meTag),
        (LUTranslate suffixDbId meTag)
        ]
      ]
    in Shoebox sbData sbTranslations


translationLength :: Shoebox -> Int
translationLength sb = length (sbTranslations sb)

-- lookup one lookup option in database
applyLookup :: Shoebox -> LookupOption -> SBDataEntry -> Either SBError (Maybe SBDataEntry)
applyLookup sb lo de = let
  (ShoeboxData dmap) = (sbData sb)
  d dbi = fromJust $ M.lookup dbi dmap
  in case lo of
    LUMorphemeBreak dbi tag -> dbQuery (d dbi) de tag
    LUKey dbi -> dbQueryKey (d dbi) de
    LUTranslate dbi tag -> dbQuery (d dbi) de tag

data TRResult = TRR {
    trrData :: SBDataEntry,
    trrLookupOption :: LookupOption
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TRResult
instance FromJSON TRResult

-- a TRLookup gives for one entry, the results of all given lookups of a specific translation
data TRLookup = TRL {
    trlSource :: SBDataEntry,
    trlTranslation :: Translation,
    trlResult :: [[TRResult]]          -- outer list, multiple lookup options, inner list morpheme break
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TRLookup
instance FromJSON TRLookup

-- get list of result entries from TRLookup
esFromTrl :: TRLookup -> [[SBDataEntry]]
esFromTrl (TRL _ _ rss) = map (map trrData) rss

-- apply one translation with multiple lookup options, gather all results
translateStep :: Shoebox -> Translation -> SBDataEntry -> TRLookup
translateStep sb tr e = let
  trns = foldl (\a lo -> case applyLookup sb lo e of
                           Right (Just e') -> case lo of
                                                (LUMorphemeBreak _ _) -> let   -- result is morphemebreak, make multiple nodes
                                                  (SbeTextArray ta) = e'
                                                  in (map (\t -> TRR (SbeText t) lo) ta) : a
                                                _ -> [ TRR e' lo ] : a -- other result, make one node per result
                           _ -> a
               ) [] (trLookup tr)
  in TRL e tr trns

data TRNode = TRN {
  trnLookup :: TRLookup,
  trnNext :: [[TRNode]]      -- matching results in trnLookup
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TRNode
instance FromJSON TRNode

translateEntry :: Shoebox -> SBDataEntry -> TRNode
translateEntry sb e = let
  trs = sbTranslations sb
  _translate [] trn = trn
  _translate (t:ts) (TRN trl@(TRL _ _ rs) _) = TRN trl (map (map (\e -> TRN (translateStep sb t e) [])) (esFromTrl trl))
  in _translate (tail trs) (TRN (translateStep sb (head trs) e) [])

