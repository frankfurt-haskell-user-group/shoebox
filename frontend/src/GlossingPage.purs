module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, stdout, stdin)
import Node.Stream (write, read, uncork, onData)
import Node.Encoding (Encoding(..))
import Data.Maybe
import Control.Monad.Eff.Class
import Node.Buffer (fromString, toString, BUFFER)

type Word = String
type Choice = Array
type Sequence = Array
type Gloss = String

type State = { word         :: Word
             , segmentation :: String
             , gloss        :: String
             , process :: ChildProcess
             }

data Query a = UpdateWord Word a
             | QueryWord a

ui :: forall eff. ChildProcess -> H.Component HH.HTML Query Unit Void (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
ui cpIn = H.component
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
                   , process : cpIn
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
            [ HH.text "Send Command" ]
        , HH.p_ [ HH.text st.segmentation ]
        , HH.p_ [ HH.text st.gloss ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        word <- H.gets _.word
        cpIn' <- H.gets _.process
        bout <- H.liftEff (fromString ("{\"cmd\" : \"" <> word <> "\"}\n") UTF8)
        _ <- H.liftEff $ write (stdin cpIn') bout (pure unit)
        b <- H.liftAff $ makeAff (\error success -> onData (stdout cpIn') success)
        s <- H.liftEff $ toString UTF8 b 
        H.modify $ _ {gloss = s}
        pure next


      {-
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
                           -> Aff (exception :: EXCEPTION, cp::CHILD_PROCESS   | eff) (Choice (Sequence (Choice Gloss)))
        querySegmentations = sequence <<< map querySegmentation

        querySegmentation :: Sequence Morpheme
                          -> Aff (exception :: EXCEPTION, cp::CHILD_PROCESS   | eff) (Sequence (Choice Gloss))
        querySegmentation = sequence <<< map queryGlosses

        queryGlosses :: Morpheme -> Aff (exception :: EXCEPTION, cp::CHILD_PROCESS   | eff) (Choice Gloss)
        queryGlosses (MorphemeLemma  lemma ) = _.meanings   <$> queryDb lemma
        queryGlosses (MorphemePrefix prefix) = _.prefixTags <$> queryDb prefix
        queryGlosses (MorphemeSuffix suffix) = _.suffixTags <$> queryDb suffix
-}
