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
import GHC.Generics
import Data.Aeson
import Data.List (find)

-- |Entry-Types in the database can have the following types
data SBDataType = SbtText | SbtTextArray | SbtNumber deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON SBDataType
instance FromJSON SBDataType

-- |Entries in the database can have tags or mnemonics
data SBDataTag = SBDataTag { sbdtMemo ::Text } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
instance ToJSON SBDataTag
instance FromJSON SBDataTag

-- |One row of a DB schema
data SBDataRowDef = SBDataRowDef { sbrdTag::SBDataTag, sbrdDesc::Text, sbrdType::SBDataType } deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON SBDataRowDef
instance FromJSON SBDataRowDef

-- |A DB schema describes all rows possible and a marker, which one is the key field
data SBDataSchema = SBDataSchema { sbdsKey::SBDataTag, sbdsRowDefs::[SBDataRowDef] } deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON SBDataSchema
instance FromJSON SBDataSchema


-- |Entries in the database can have the following values
data SBDataEntry = SbeText Text | SbeTextArray [Text] | SbeNumber Int deriving (Show, Ord, Read, Eq, Data, Typeable, Generic)
instance ToJSON SBDataEntry
instance FromJSON SBDataEntry

-- |One Data Row
data SBDataRow = SBDataRow { sbdrTag::SBDataTag, sbdrEntry::SBDataEntry } deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON SBDataRow
instance FromJSON SBDataRow

-- |A database consists of a schema and the actual data-fields, the tag and type of the key is encoded in the schema def
-- In the data rows, of the map, the key is not included, it is included in the key of the map
data SBDatabase = SBDatabase { sbdbDescription::Text, sbdbSchema::SBDataSchema, sbdbRows::M.Map SBDataEntry [SBDataRow] } deriving (Show, Read, Eq, Data, Typeable, Generic)
instance FromJSONKey SBDataEntry 
instance ToJSONKey SBDataEntry 
instance ToJSON SBDatabase
instance FromJSON SBDatabase

-- |A database identifier contains a shortcut for the database and a description
data SBDbIdent = SBDbIdent { sbdiName::Text } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
instance FromJSONKey SBDbIdent 
instance ToJSONKey SBDbIdent 
instance ToJSON SBDbIdent
instance FromJSON SBDbIdent

-- |All data of one Shoebox in a map from ident to database
data ShoeboxData = ShoeboxData (M.Map SBDbIdent SBDatabase) deriving (Show, Read, Eq, Data, Typeable, Generic)
instance ToJSON ShoeboxData
instance FromJSON ShoeboxData


-- |Errors for SB Data
data SBError = SbErrorUpdateWrongKeyType
        | SbErrorUpdateWrongDataType
        | SbErrorUpdateDataRowWithKeyTag

        | SbErrorQueryWrongKeyType
        | SbErrorQueryWrongDataTag

        |SbErrorDataParsingFailed Text

        |SbErrorQueryRuleInputNotText 

        deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON SBError
instance FromJSON SBError

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

-- |Query database to check, if rows exists for an entry, gives key back as entry
dbQueryKey :: SBDatabase -> SBDataEntry -> Either SBError (Maybe SBDataEntry)
dbQueryKey db key = if not (dbCheckKeyType db key)
    then Left SbErrorQueryWrongKeyType
    else case M.lookup key (sbdbRows db) of
        Just rs -> Right (Just key)
        Nothing -> Right Nothing

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

-- some db idents for clarity
textDbId = SBDbIdent "textDB" 
parsingDbId = SBDbIdent "parsingDB" 
lexDbId = SBDbIdent "lexDB"
suffixDbId = SBDbIdent "suffixDB" 

-- |Test database for the start, text content
textDB = SBDatabase 
            "text database, containing complete sentences and translations"
            (SBDataSchema 
                refTag
                [
                    SBDataRowDef refTag "unique reference as id number" SbtNumber,
                    SBDataRowDef txTag "complete sentence as text" SbtText,
                    SBDataRowDef mbTag "morpheme break of sentence" SbtText,
                    SBDataRowDef glTag "gloss: meanings and translations from databases" SbtText,
                    SBDataRowDef frTag "freetext translation" SbtText
                ])
            (M.fromList [])

parsingDB = SBDatabase
            "database, containing break-downs of words, for parsing"
            (SBDataSchema 
                flTag
                [
                    SBDataRowDef flTag "full text, to be broken down" SbtText,
                    SBDataRowDef mbTag "morpheme break of text" SbtText
                ])
            (M.fromList [])

lexDB = SBDatabase
             "database, containing lexical elements and translations (meanings)"
            (SBDataSchema 
                leTag
                [
                    SBDataRowDef leTag "lexical value" SbtText,
                    SBDataRowDef meTag "meaning" SbtTextArray,
                    SBDataRowDef coTag "comment" SbtText
                ])
            (M.fromList [(
                            SbeText "wowdy", 
                            [ SBDataRow meTag (SbeText "cool wowdy") ]
                        )])

suffixDB = lexDB { sbdbDescription = "database, containing prefixes and suffixes"}

newShoeboxData = ShoeboxData $ M.fromList [
        (textDbId, textDB),
        (parsingDbId, parsingDB),
        (lexDbId, lexDB),
        (suffixDbId, suffixDB)
    ]

