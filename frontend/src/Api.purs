module Api
    ( Morpheme(..)
    , Lemma
    , Prefix
    , Suffix
    , queryDb
    ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, Json, (.?), decodeJson)
import Data.Either (Either(Left), either)
import Network.HTTP.Affjax (AJAX, post)

type MorphemeBreaks = Array Morpheme
type Lemma = String
type Meaning = String
type Prefix = String
type PrefixTag = String
type Suffix = String
type SuffixTag = String

type DbEntries =
    { morphemeBreaks :: Array MorphemeBreaks
    , meanings       :: Array Meaning
    , prefixTags     :: Array PrefixTag
    , suffixTags     :: Array SuffixTag
    }

data Morpheme = MorphemeLemma  Lemma
              | MorphemePrefix Prefix
              | MorphemeSuffix Suffix

instance decodeMorpheme :: DecodeJson Morpheme where
    decodeJson json = do
        obj <- decodeJson json
        (construct =<< obj .? "tag") <*> obj .? "contents"
      where
        construct "MorphemeLex"    = pure MorphemeLemma
        construct "MorphemePrefix" = pure MorphemePrefix
        construct "MorphemeSuffix" = pure MorphemeSuffix
        construct _                = Left "unknown tag"

decodeDbEntries :: Json -> Either String DbEntries
decodeDbEntries json = do
    obj            <- decodeJson json
    morphemeBreaks <- obj .? "morphemeBreaks"
    meanings       <- obj .? "meanings"
    prefixTags     <- obj .? "prefixTags"
    suffixTags     <- obj .? "suffixTags"
    pure { morphemeBreaks
         , meanings
         , prefixTags
         , suffixTags
         }

queryDb :: forall eff. String -> Aff (ajax :: AJAX | eff) DbEntries
queryDb key = do
    res <- post url $ "\"" <> key <> "\""
    either (throwError <<< error) pure $ decodeDbEntries res.response
  where
    url = "http://localhost:3000/api/query/word"

