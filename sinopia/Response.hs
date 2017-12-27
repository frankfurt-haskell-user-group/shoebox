import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | response to file operations
data FileResponse = CurrentDBChanged Text -- ^ the DB in use just changed 
    | AvailableDBs [Text] -- ^ list of DB's on disk 
    | OpenedDB Text -- ^ just opened the DB 
    | CreatedDB Text -- ^ just created the DB 
    | DeletedDB Text -- ^ just deleted the DB 
    | SavedDB Text -- ^ just saved the DB 
    deriving (Eq, Read, Show)

-- | response to database queries
data QueryResponse = DbInfo Text -- ^ detailed DB info as JSON text 
    | DbQuery Text -- ^ query answer as JSON text 
    deriving (Eq, Read, Show)

-- | response to commands
data Response = NoResponse -- ^ response to NoOP command (is this needed?) 
    | ResFr FileResponse -- ^ response to a file command 
    | ResQuery QueryResponse -- ^ response to a query command 
    | TestAnswer Text -- ^ arbitrary test answer as text 
    deriving (Eq, Read, Show)

instance Serialise FileResponse where
    encode (CurrentDBChanged v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    encode (AvailableDBs v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (OpenedDB v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (CreatedDB v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    encode (DeletedDB v1) = encodeListLen 2 <>  encode (4::Int) <> encode v1
    encode (SavedDB v1) = encodeListLen 2 <>  encode (5::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (CurrentDBChanged <$> decode)
            1 -> (AvailableDBs <$> decode)
            2 -> (OpenedDB <$> decode)
            3 -> (CreatedDB <$> decode)
            4 -> (DeletedDB <$> decode)
            5 -> (SavedDB <$> decode)

instance Serialise QueryResponse where
    encode (DbInfo v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1
    encode (DbQuery v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (DbInfo <$> decode)
            1 -> (DbQuery <$> decode)

instance Serialise Response where
    encode (NoResponse) = encodeListLen 1 <>  encode (0::Int) 
    encode (ResFr v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (ResQuery v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (TestAnswer v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoResponse)
            1 -> (ResFr <$> decode)
            2 -> (ResQuery <$> decode)
            3 -> (TestAnswer <$> decode)

