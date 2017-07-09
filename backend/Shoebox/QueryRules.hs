{-# LANGUAGE OverloadedStrings #-}
module Shoebox.QueryRules where

import Text.Printf
import Data.List (intercalate)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe

import Shoebox.Data

data QRFlag = BreakFlag

-- |A query rule, goes from a source tag, towards a target tag, with a lookup in a specific DB
data QueryRule = QueryRule {
    qrSourceTag :: SBDataTag,
    qrTargetTag :: SBDataTag,
    qrSearchDBs :: [(SBDatabase, SBDataTag, SBDataTag, [QRFlag])]
    }

-- |A set of query rules defines the shoebox
data ShoeBox = ShoeBox [QueryRule]

shoeBoxFromDB :: SBDatabase -> SBDatabase -> SBDatabase -> ShoeBox
shoeBoxFromDB lDB pDB sDB = ShoeBox [
    QueryRule txTag mbTag [
        (pDB, flTag, mbTag, [BreakFlag]),
        (lDB, leTag, leTag, []),
        (sDB, leTag, leTag, [])
        ],
    QueryRule mbTag glTag [
        (lDB, leTag, meTag, []),
        (sDB, leTag, meTag, [])
        ]
    ]


{-

breakTX :: Text -> ShoeSegmentationDB -> [MorphemeBreak]
breakTX textEl segmentationDB =
  fromMaybe [MB [MorphemeLex textEl]]
    (M.lookup textEl segmentationDB)

lookupMB :: MorphemeBreak -> ShoeLexiconDB -> ShoeSuffixDB -> ShoePrefixDB -> [Choice]
lookupMB (MB mbs) lexiconDB suffixDB prefixDB = map go mbs
  where
    go (MorphemeLex l)    = MeaningChoice $ fromMaybe [] (M.lookup l lexiconDB)
    go (MorphemeSuffix s) = AbbreviationChoice $ fromMaybe [] (M.lookup s suffixDB)qr
    go (MorphemePrefix p) = AbbreviationChoice $ fromMaybe [] (M.lookup p prefixDB)

gloss :: Text -> ShoeDB -> [InterlinearBlock]
gloss textEl (lexiconDB,suffixDB,prefixDB,segmentationDB) = do
  morphemeBreak <- breakTX textEl segmentationDB
  let glosses = GL (lookupMB morphemeBreak lexiconDB suffixDB prefixDB)
  return $ ILB (TX textEl) morphemeBreak glosses

intlx :: [Text] -> ShoeDB -> [[InterlinearBlock]]
intlx xs shoeDB =  map (`gloss` shoeDB) xs

intl :: Text -> ShoeDB -> [[InterlinearBlock]]
intl s = intlx (splitOn " " s)

pp :: [InterlinearBlock] -> Text
pp = undefined

removePunc :: ShoeDB -> Text -> Text
removePunc = undefined

-- OLD 

import DatabaseParser





data Decision = Abort | Decided Gloss | Skip | NewGloss [Gloss]
  deriving Show

decide :: TextEl -> Choice -> IO Decision
decide textEl (MeaningChoice choices) = do
  printf "Multiple glosses found, please choose:\n%s\n"
             (showChoices choices)
  printf "(x) none of these, enter new gloss\n"
  printf "(s) skip this gloss\n"
  printf "(m) annotate manually\n"
  printf "(a) abort interlinearization\n"
  printf "(t) try applying external script\n"
  line <- getLine
  case readMaybe line :: Maybe Int of
    Nothing ->
      let action = head line
      in case action of
           'x' -> addNewGloss choices
           's' -> return Skip
           'm' -> annotateManually
           'a' -> return Abort
           't' -> externalShellScript textEl
           _ -> do
             printf "unrecognized option, try again.\n"
             decide textEl (MeaningChoice choices)
    Just n | n == 0 || n > length choices -> do
               printf "Choice out of range, try again.\n"
               decide textEl (MeaningChoice choices)
           | otherwise ->
               return $ Decided $ choices !! (n-1)

addNewGloss :: [Gloss] -> IO Decision
addNewGloss glosses = do
  putStrLn "Enter a new gloss:"
  newGloss <- T.getLine
  putStrLn "Enter name of the database for the new gloss:"
  db <- T.getLine
  return $ NewGloss $ Gloss db newGloss : glosses

externalShellScript :: TextEl -> IO Decision
externalShellScript textEl = do
  putStrLn "Enter the path to the external shell script:"
  path <- getLine
  putStrLn "Enter name of the database for the new gloss:"
  db <- T.getLine
  (Just stdin, Just stdout,_,_) <- createProcess (proc path [])
    { std_in = CreatePipe
    , std_out = CreatePipe
    }
  T.hPutStr stdin textEl
  glosses <- T.splitOn "," <$> T.hGetLine stdout
  return $ NewGloss $ Gloss db <$> glosses

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  []        -> Nothing
  ((x,_):_) -> Just x

annotateManually :: IO Decision
annotateManually = undefined

showChoices :: [Gloss] -> String
showChoices l = intercalate ", " $ do
  (n,Gloss _ g) <- zip [(1::Int)..] l
  return $ concat ["(", show n, ") ", T.unpack g]


importLexDBElem :: Text -> DBElem
importLexDBElem = undefined

importSuffDBElem :: Text -> DBElem
importSuffDBElem = undefined

importPrefixDBElem :: ShoeSuffixDB -> Text -> (Text, [DBElem])
importPrefixDBElem = undefined

importSegmentationDBElem :: ShoeSuffixDB -> ShoePrefixDB -> Text -> (Text, [DBElem])
importSegmentationDBElem = undefined
-}

--genuuid :: IO Data.UUID.Types.Internal.UUID
--genuuid = nextRandom

--isUUID :: (Typeable a) => a -> Bool
--isUUID n = typeOf n == typeOf nextRandom


