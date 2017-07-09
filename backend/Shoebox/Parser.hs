{-# LANGUAGE OverloadedStrings #-}

module Shoebox.Parser (
--  loadShoeDB  -- loads data from file
  parseDB
  ) where

import           Data.Monoid ((<>))
import           Text.Parsec (Parsec)
import qualified Text.Parsec as P
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Map as M

import Shoebox.Data

-- base delimiters and sections, to cope with \n in between
-- the rules are as follows:
-- a "line" starts with a newline + "\"
-- a "section" separator is a newline followed by a line starter

spaces :: Parsec Text SBDatabase String
spaces = P.many (P.oneOf " \t")     -- own spaces without newline

parseSecSep :: Parsec Text SBDatabase [Char] 
parseSecSep = P.try (P.newline >> P.many1 P.newline)

parseElemSep :: Parsec Text SBDatabase Char 
parseElemSep = P.try (P.newline >> (P.lookAhead (P.char '\\')))

parseElemChar :: Parsec Text SBDatabase Char
parseElemChar = do
  P.try (P.noneOf ['\n', ';'] )
  P.<|> 
  (P.try (P.newline >> P.lookAhead (P.noneOf ['\\', '\n']) ))

parseElemCharBK :: Parsec Text SBDatabase Char
parseElemCharBK = do
  P.try (P.noneOf ['\n', ';', '-', '='] )
  P.<|> 
  (P.try (P.newline >> P.lookAhead (P.noneOf ['\\', '\n']) ))

parseAllDataRows :: Parsec Text SBDatabase [[SBDataRow]]
parseAllDataRows = P.sepEndBy1 parseRecord parseSecSep

parseRecord :: Parsec Text SBDatabase [SBDataRow]
parseRecord = P.sepEndBy1 parseRow parseElemSep

parseRow :: Parsec Text SBDatabase SBDataRow 
parseRow = do
  db <- P.getState

  P.string "\\"
  mem <- P.many1 (P.noneOf [' ', '\t'])
  spaces 

  let tag = SBDataTag (T.pack mem)
  if dbCheckTag db tag 
    then SBDataRow <$> pure tag <*> parseEntry (dbTagType db tag)
    else SBDataRow <$> pure tag <*> parseTextEntry 

parseEntry :: SBDataType -> Parsec Text SBDatabase SBDataEntry  
parseEntry t = case t of
                  SbtText -> parseTextEntry
                  SbtTextArray -> parseTextArrayEntry
                  SbtNumber -> parseNumberEntry

parseTextEntry :: Parsec Text SBDatabase SBDataEntry  
parseTextEntry = do
  t <- P.many parseElemChar
  return $ SbeText (T.pack t)

parseTextArrayEntry :: Parsec Text SBDatabase SBDataEntry  
parseTextArrayEntry = do
  ts <- P.sepEndBy (P.many parseElemChar) (P.char ';' >> spaces)
  return $ SbeTextArray (map T.pack ts)

parseNumberEntry :: Parsec Text SBDatabase SBDataEntry  
parseNumberEntry = do
  t <- P.many parseElemChar
  return $ SbeNumber (read t)

parseDB :: Text -> SBDatabase -> (Either SBError [[SBDataRow]])
parseDB inText db = case P.runParser parseAllDataRows db "" inText of
                        Left m -> Left (SbErrorDataParsingFailed ((T.pack . show) m))
                        Right val -> Right val

