{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-|
Module      : Shoebox.Data
Description : Data definitions for shoebox
Copyright   : (c) Frankfurt Haskell User Group, 2017
License     : Apache 2.0
Maintainer  : 

In this module, the database structure for shoebox is defined as a configurable database 
format with mostly freetext fields. 
-}
module Shoebox.Data where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Typeable
import Data.Data
import Data.Aeson
import GHC.Generics
import Data.List (find)

-- |Entry-Types in the database can have the following types
data SBDataType = SbtText | SbtTextArray | SbtNumber deriving (Show, Read, Eq, Data, Typeable, Generic)

-- |Entries in the database can have tags or mnemonics
data SBDataTag = SBDataTag { sbdtMemo ::Text } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- |One row of a DB schema
data SBDataRowDef = SBDataRowDef { sbrdTag::SBDataTag, sbrdDesc::Text, sbrdType::SBDataType } deriving (Show, Read, Eq, Data, Typeable, Generic)

-- |A DB schema describes all rows possible and a marker, which one is the key field
data SBDataSchema = SBDataSchema { sbdsKey::SBDataTag, sbdsRowDefs::[SBDataRowDef] } deriving (Show, Read, Eq, Data, Typeable, Generic)


-- |Entries in the database can have the following values
data SBDataEntry = SbeText Text | SbeTextArray [Text] | SbeNumber Int deriving (Show, Ord, Read, Eq, Data, Typeable, Generic)

-- |One Data Row
data SBDataRow = SBDataRow { sbdrTag::SBDataTag, sbdrEntry::SBDataEntry } deriving (Show, Read, Eq, Data, Typeable, Generic)

-- |A database consists of a schema and the actual data-fields, the tag and type of the key is encoded in the schema def
-- In the data rows, of the map, the key is not included, it is included in the key of the map
data SBDatabase = SBDatabase { sbdbSchema::SBDataSchema, sbdbRows::M.Map SBDataEntry [SBDataRow] } deriving (Show, Read, Eq, Data, Typeable, Generic)

-- |Errors for SB Data
data SBError = SbErrorUpdateWrongKeyType
        | SbErrorUpdateWrongDataType
        | SbErrorUpdateDataRowWithKeyTag

        | SbErrorQueryWrongKeyType
        | SbErrorQueryWrongDataTag

        |SbErrorDataParsingFailed Text 

        deriving (Show, Read, Eq, Data, Typeable, Generic)

-- |Type of a tag in DB, assumed to be present in Schema
dbTagType :: SBDatabase -> SBDataTag -> SBDataType
dbTagType db tag = let 
    [rd] = filter (\r -> (sbrdTag r) == tag) ((sbdsRowDefs . sbdbSchema) db) 
    in (sbrdType rd)

-- |Check, if tag is allowed in DB, gives also ok (True) for key tag
dbCheckTag :: SBDatabase -> SBDataTag -> Bool
dbCheckTag db tag = let
    rds = filter (\r -> (sbrdTag r) == tag) ((sbdsRowDefs . sbdbSchema) db)
    in case rds of 
        [] -> False
        [rd] -> True

-- |Check, if data type is correct one for this tag
dbCheckTagType :: SBDatabase -> SBDataTag -> SBDataType -> Bool
dbCheckTagType db tag t = let
    rds = filter (\r -> (sbrdTag r) == tag) ((sbdsRowDefs . sbdbSchema) db)
    in case rds of 
        [] -> False
        [rd] -> (sbrdType rd) == t 

-- |Get type from DataEntry, for checks
dbEntryType :: SBDataEntry -> SBDataType
dbEntryType entry = case entry of
    SbeText t -> SbtText
    SbeTextArray a -> SbtTextArray
    SbeNumber i -> SbtNumber

-- |Check, if data entry is correct key type 
dbCheckKeyType :: SBDatabase -> SBDataEntry -> Bool
dbCheckKeyType db entry = dbCheckTagType db ( (sbdsKey . sbdbSchema) db) (dbEntryType entry)

-- helper function, to update one row in a record (list)
updateRowData :: SBDataEntry -> SBDataRow -> M.Map SBDataEntry [SBDataRow] -> M.Map SBDataEntry [SBDataRow]
updateRowData key row rm = let
    rows = case M.lookup key rm of 
            Just rs -> rs
            Nothing -> []
    rows' = map (\r -> if (sbdrTag r) == (sbdrTag row) then row else r) rows
    rows'' = if (sbdrTag row) `elem` (map sbdrTag rows') then rows' else (rows' ++ [row])
    in M.insert key rows'' rm 

-- helper functioon, to query row data
queryRowData :: SBDataEntry -> SBDataTag -> M.Map SBDataEntry [SBDataRow] -> Maybe SBDataRow
queryRowData key tag rm = do
    rows <- M.lookup key rm  
    rval <- find (\r -> (sbdrTag r) == tag) rows
    return rval 

-- |Update database with entry
dbUpdate :: SBDatabase -> SBDataEntry -> SBDataRow -> Either SBError SBDatabase
dbUpdate db key row = if not (dbCheckKeyType db key)
    then Left SbErrorUpdateWrongKeyType
    else if not (dbCheckTagType db (sbdrTag row) ((dbEntryType . sbdrEntry) row))
            then Left SbErrorUpdateWrongDataType
            else if (sbdrTag row) == ((sbdsKey . sbdbSchema) db) -- key tag row should not be in data
                then Left SbErrorUpdateDataRowWithKeyTag
                else Right $ db { sbdbRows = updateRowData key row (sbdbRows db)  }

-- |Query database for an entry
dbQuery :: SBDatabase -> SBDataEntry -> SBDataTag -> Either SBError (Maybe SBDataEntry)
dbQuery db key tag = if not (dbCheckKeyType db key)
    then Left SbErrorQueryWrongKeyType
    else if not (dbCheckTag db tag)
        then Left SbErrorQueryWrongDataTag
        else Right $ fmap sbdrEntry (queryRowData key tag (sbdbRows db)) 

-- |Insert from records, ignores records without valid key or valid tag or valid data
dbInsertFromRecords :: SBDatabase -> [[SBDataRow]] -> SBDatabase
dbInsertFromRecords db recs = let 
    isKey db row = (sbdrTag row) == ((sbdsKey . sbdbSchema) db)
    findKey rec db = map sbdrEntry (filter (isKey db) rec)
    findRec rec = filter (\r -> not (isKey db r)) rec  
    insertRecord db rec = case findKey rec db of
        [entry] -> foldl (\db r -> case dbUpdate db entry r of
                                        Right db' -> db'
                                        Left _ -> db) db (findRec rec)
        [] -> db
    in foldl insertRecord db recs


-- some data tags, for clarity

refTag = SBDataTag "ref"
txTag = SBDataTag "tx"
mbTag = SBDataTag "mb"
glTag = SBDataTag "gl"
frTag = SBDataTag "fr"

flTag = SBDataTag "fl"

leTag = SBDataTag "le"
meTag = SBDataTag "me"
coTag = SBDataTag "co"

-- |Test database for the start, text content
textDB = SBDatabase 
            (SBDataSchema 
                refTag
                [
                    SBDataRowDef refTag "unique reference as id number" SbtNumber,
                    SBDataRowDef txTag "complete sentence as text" SbtText,
                    SBDataRowDef mbTag "morphene break of sentence" SbtText,
                    SBDataRowDef glTag "gloss: meanings and translations from databases" SbtText,
                    SBDataRowDef frTag "freetext translation" SbtText
                ])
            (M.fromList [])

parsingDB = SBDatabase
            (SBDataSchema 
                flTag
                [
                    SBDataRowDef flTag "full text, to be broken down" SbtText,
                    SBDataRowDef mbTag "morphene break of text" SbtText
                ])
            (M.fromList [])

lexDB = SBDatabase
            (SBDataSchema 
                leTag
                [
                    SBDataRowDef leTag "lexical value" SbtText,
                    SBDataRowDef meTag "meaning" SbtTextArray,
                    SBDataRowDef coTag "comment" SbtText
                ])
            (M.fromList [])

suffixDB = lexDB

data ShoeDB = ShoeDB { shoeDBTxt :: SBDatabase, shoeDBParsing :: SBDatabase, shoeDBLex :: SBDatabase, shoeDBSuffix :: SBDatabase } 
              deriving (Show, Read, Eq, Data, Typeable, Generic)

newEmptyDB = ShoeDB textDB parsingDB lexDB suffixDB

{-

-- The data types
-- --------------

-- The input (raw) data without any further processing is a TextLine
newtype TextLine = TX Text deriving (Show, Read, Eq, Data, Typeable, Generic)

-- A TextLine is broken up into Morpheme's, which can be lexical elements, suffixed or prefixes
-- MorphemeBreak is a segmented word.
newtype MorphemeBreak = MB [Morpheme] deriving (Show, Read, Eq, Data, Typeable, Generic)
data Morpheme = MorphemeLex Text | MorphemeSuffix Text | MorphemePrefix Text deriving (Show, Read, Eq, Data, Typeable, Generic)

-- A GlossLine summarizes all Glosses, which are meanings attached to Morphemes
newtype GlossLine = GL [Choice] deriving (Show, Read, Eq, Data, Typeable, Generic)
data Choice = MeaningChoice [Text] | AbbreviationChoice [Text] deriving (Show, Read, Eq, Data, Typeable, Generic)

-- An interlinear block is a "categorized" textline, inluding its morphemes and glosses
data InterlinearBlock = ILB TextLine MorphemeBreak GlossLine deriving (Show, Read, Eq, Data, Typeable, Generic)

-- The databases
-- -------------

type Word = Text
type Lemma = Text
type Meaning = Text
type Suffix = Text
type SuffixTag = Text
type Prefix = Text
type PrefixTag = Text

type ShoeSegmentationDB = M.Map Word [MorphemeBreak] -- segmentation
type ShoeLexiconDB = M.Map Lemma [Meaning] -- base words, lexicon
type ShoeSuffixDB  = M.Map Suffix [SuffixTag] -- suffixes
type ShoePrefixDB  = M.Map Prefix [PrefixTag] -- prefixes
type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoePrefixDB, ShoeSegmentationDB)

instance ToJSON MorphemeBreak
instance ToJSON Morpheme
instance ToJSON GlossLine
instance ToJSON Choice
instance ToJSON InterlinearBlock
instance ToJSON TextLine

instance FromJSON MorphemeBreak
instance FromJSON Morpheme
instance FromJSON GlossLine
instance FromJSON Choice
instance FromJSON InterlinearBlock
instance FromJSON TextLine

-}