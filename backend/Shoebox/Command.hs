module Shoebox.Command where

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data FileCommand = GetCurrentDB
    | GetAvailableDBs
    | CreateDB Text
    | DeleteDB Text
    | OpenDB Text
    | SaveDB
    | SaveDBAs Text
    deriving (Eq, Read, Show)

data QueryCommand = DbInfo
    | DbQuery Text
    deriving (Eq, Read, Show)

data Command = NoCommand
    | CmdFc FileCommand
    | CmdQuery QueryCommand
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
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoCommand)
            1 -> (CmdFc <$> decode)
            2 -> (CmdQuery <$> decode)

