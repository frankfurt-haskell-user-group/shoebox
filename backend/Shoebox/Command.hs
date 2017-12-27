module Shoebox.Command where

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | commands for file operations
data FileCommand = GetCurrentDB -- ^ show DB in use 
    | GetAvailableDBs -- ^ get list of all DB's 
    | CreateDB Text -- ^ create a new DB 
    | DeleteDB Text -- ^ delete an existing DB 
    | OpenDB Text -- ^ open an existing DB 
    | SaveDB -- ^ save the DB in use 
    | SaveDBAs Text -- ^ save the DB in use with a new name 
    deriving (Eq, Read, Show)

-- | commands for database query operations
data QueryCommand = DbInfo -- ^ get detailed info on a database 
    | DbQuery Text -- ^ query a database for an entry 
    deriving (Eq, Read, Show)

-- | all possible commands for the shoebox module
-- gathered from the different command sub types
data Command = NoCommand -- ^ no action requested 
    | CmdFc FileCommand -- ^ one of the file commands 
    | CmdQuery QueryCommand -- ^ one of the query commands 
    | RunTest Text -- ^ arbitrary text, send as test command 
    deriving (Eq, Read, Show)

instance Serialise FileCommand where
    encode (GetCurrentDB) = encodeListLen 1 <>  encode (0::Int) 
    encode (GetAvailableDBs) = encodeListLen 1 <>  encode (1::Int) 
    encode (CreateDB v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (DeleteDB v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    encode (OpenDB v1) = encodeListLen 2 <>  encode (4::Int) <> encode v1
    encode (SaveDB) = encodeListLen 1 <>  encode (5::Int) 
    encode (SaveDBAs v1) = encodeListLen 2 <>  encode (6::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure GetCurrentDB)
            1 -> (pure GetAvailableDBs)
            2 -> (CreateDB <$> decode)
            3 -> (DeleteDB <$> decode)
            4 -> (OpenDB <$> decode)
            5 -> (pure SaveDB)
            6 -> (SaveDBAs <$> decode)

instance Serialise QueryCommand where
    encode (DbInfo) = encodeListLen 1 <>  encode (0::Int) 
    encode (DbQuery v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure DbInfo)
            1 -> (DbQuery <$> decode)

instance Serialise Command where
    encode (NoCommand) = encodeListLen 1 <>  encode (0::Int) 
    encode (CmdFc v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (CmdQuery v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (RunTest v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoCommand)
            1 -> (CmdFc <$> decode)
            2 -> (CmdQuery <$> decode)
            3 -> (RunTest <$> decode)

