module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

import Api (Morpheme(..), queryDb)

type Word = String
type Choice = Array
type Sequence = Array
type Gloss = String

type State = { word         :: Word
             , segmentation :: String
             , gloss        :: String
             }

data Query a = UpdateWord Word a
             | QueryWord a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
ui = H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = { word         : ""
                   , segmentation : ""
                   , gloss        : ""
                   }

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
        [ HH.h1_ [ HH.text "Shoebox" ]
        , HH.input
            [ HP.value st.word
            , HE.onValueInput $ HE.input UpdateWord
            ]
        , HH.button
            [ HP.disabled $ st.word == mempty
            , HE.onClick $ HE.input_ QueryWord
            ]
            [ HH.text "Query Word" ]
        , HH.p_ [ HH.text st.segmentation ]
        , HH.p_ [ HH.text st.gloss ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        word <- H.gets _.word
        result <- H.liftAff $ queryDb word
        if result.meanings /= mempty
           then H.modify $ _ { segmentation = showChoice result.meanings
                             , gloss        = showChoice result.meanings
                             }
           else do
               glosses <- H.liftAff $ querySegmentations result.morphemeBreaks
               H.modify $ _ { segmentation = showSegmentations result.morphemeBreaks
                            , gloss        = showGlosses glosses
                            }
        pure next
      where
        showChoice :: Choice String -> String
        showChoice = intercalate "/"

        showSeq :: Sequence String -> String
        showSeq = intercalate "-"

        showSegmentations :: Choice (Sequence Morpheme) -> String
        showSegmentations = showChoice <<< map (showSeq <<< map showMorpheme)

        showMorpheme :: Morpheme -> String
        showMorpheme (MorphemeLemma  lemma ) = lemma
        showMorpheme (MorphemePrefix prefix) = prefix
        showMorpheme (MorphemeSuffix suffix) = suffix

        showGlosses :: Choice (Sequence (Choice Gloss)) -> String
        showGlosses = showChoice <<< map showSeq <<< (map $ map showChoice)

        querySegmentations :: Choice (Sequence Morpheme)
                           -> Aff (ajax :: AJAX | eff) (Choice (Sequence (Choice Gloss)))
        querySegmentations = sequence <<< map querySegmentation

        querySegmentation :: Sequence Morpheme
                          -> Aff (ajax :: AJAX | eff) (Sequence (Choice Gloss))
        querySegmentation = sequence <<< map queryGlosses

        queryGlosses :: Morpheme -> Aff (ajax :: AJAX | eff) (Choice Gloss)
        queryGlosses (MorphemeLemma  lemma ) = _.meanings   <$> queryDb lemma
        queryGlosses (MorphemePrefix prefix) = _.prefixTags <$> queryDb prefix
        queryGlosses (MorphemeSuffix suffix) = _.suffixTags <$> queryDb suffix

