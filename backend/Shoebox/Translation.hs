{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric  #-}
module Shoebox.Translation where

import Text.Printf
import Data.List as L
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe

import Data.Data
import Data.Typeable
import GHC.Generics
import Data.Aeson

import Control.Applicative
import Data.Functor
import Data.Traversable

import Shoebox.Data

data LookupOption = LUMorphemeBreak SBDbIdent SBDataTag -- database contains morpheme breaks at tag, source will be splitted in mulitple parts
               | LUKey SBDbIdent -- keys of database contains the tags, we simply use as is
               | LUTranslate SBDbIdent SBDataTag -- target tag contains translation of key tag
               deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON LookupOption
instance FromJSON LookupOption

-- each translation rule transforms from source to target, with multiple lookup options
data TranslationRule = TranslationRule {
  trTextDB :: SBDbIdent,
  trSourceTag :: SBDataTag,
  trTargetTag :: SBDataTag,
  trLookup :: [LookupOption]
} deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TranslationRule
instance FromJSON TranslationRule

-- |The data plus a set of query rules defines the shoebox
data Shoebox = Shoebox {
    sbData :: ShoeboxData,
    sbTranslationRules  :: [TranslationRule] 
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Shoebox
instance FromJSON Shoebox

newShoebox :: Shoebox
newShoebox = let
    sbData = newShoeboxData
    sbTranslationRules = [
      TranslationRule textDbId txTag mbTag [
        (LUMorphemeBreak parsingDbId bkTag),
        (LUKey lexDbId),
        (LUKey suffixDbId) 
        ]
      , TranslationRule textDbId mbTag glTag [
        (LUTranslate lexDbId meTag),
        (LUTranslate suffixDbId meTag)
        ]
      ]
    in Shoebox sbData sbTranslationRules

-- try one lookup option, data lookup
tryLookupOption :: Shoebox -> LookupOption -> SBDataEntry -> Either SBError (Maybe SBDataEntry)
tryLookupOption sb lo de = let
  (ShoeboxData dmap) = (sbData sb)
  d dbi = fromJust $ M.lookup dbi dmap
  in case lo of
    LUMorphemeBreak dbi tag -> dbQuery (d dbi) de tag
    LUKey dbi -> dbQueryKey (d dbi) de
    LUTranslate dbi tag -> dbQuery (d dbi) de tag

data LookupResult = LR {
    lrData :: SBDataEntry,
    lrLookupOption :: LookupOption
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON LookupResult
instance FromJSON LookupResult

-- data type for double list, where outer list is describing multiple results,
-- inner list is describing a split into multiple options of one result
data DList a = DL { getDList :: [[a]]} deriving (Show, Read, Eq, Data, Typeable, Generic)

instance Functor DList where
  fmap f (DL iss) = DL ( (map . map) f iss)

instance Foldable DList where
  foldr f z (DL iss) = L.foldr f z (L.concat iss)

instance Traversable DList where
  traverse f (DL []) = DL <$> (pure [])
  traverse f (DL (is:iss)) = DL <$> ( (:) <$> t f is <*> t' f iss) where
    t' f [] = pure []
    t' f (is':iss') = (:) <$> t f is' <*> t' f iss'
    t f [] = pure []
    t f (i':is') = (:) <$> f i' <*> t f is'

instance ToJSON a => ToJSON (DList a)
instance FromJSON a => FromJSON (DList a)


-- a Translation is a result of a translation rule for one entry
data Translation = TRL {
    trlEntry :: SBDataEntry,
    trlTranslationRule :: TranslationRule,
    trlResults :: DList LookupResult          -- outer list, multiple lookup options, inner list morpheme break
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON Translation
instance FromJSON Translation

-- apply one translation with multiple lookup options, gather all results, find structure
-- this is the central routine, which does DB lookups and builds translation structure
translateStep :: Shoebox -> TranslationRule -> SBDataEntry -> Translation
translateStep sb r e = let
  lrs = foldl (\a lo -> case tryLookupOption sb lo e of
                           Right (Just e') -> case lo of
                                                (LUMorphemeBreak _ _) -> let   -- result is morphemebreak, make multiple nodes
                                                  (SbeTextArray ta) = e'
                                                  in (map (\t -> LR (SbeText t) lo) ta) : a
                                                _ -> [ LR e' lo ] : a -- other result, make one node per result
                           _ -> a
               ) [] (trLookup r)
  in TRL e r (DL lrs)


data TRNode = TRN {
  trnTranslation :: Translation,
  trnNext :: DList TRNode
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TRNode
instance FromJSON TRNode

translateEntry :: Shoebox -> SBDataEntry -> TRNode
translateEntry sb e = let
  rules = sbTranslationRules sb
  tr (r:[]) e = TRN (translateStep sb r e) (DL [])
  tr (r:rs) e = let
    trl@(TRL _ _ rss) = translateStep sb r e
    in TRN trl (fmap (\lr -> tr rs (lrData lr)) rss)
  in tr rules e
