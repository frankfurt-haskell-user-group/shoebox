module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Network.HTTP.Affjax (AJAX)
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, stdout, stdin)
import Node.Stream (write, read, uncork, onData)
import Node.Encoding (Encoding(..))
import Data.Maybe (Maybe(..))
import Node.Buffer (fromString, toString, BUFFER)
import Halogen.HTML (ClassName(..))
import Bootstrap as HB

import Api (queryAPI)

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
             | OpenDatabase a
             | SaveDatabase a
             | NewDatabase a
             | RemoveDatabase a

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
    render st =  

      -- outer container, fluid (see bootstrap)
      HH.div [HP.class_ (ClassName "container")] [  

        -- top row 
        HH.div [HP.classes [HB.row, (ClassName "top-row")]] [
          -- left side, db text
          HH.div [HP.classes [HB.colSm6]] [
            HH.h3_ [ HH.small_ [ HH.text "selected database: " ], HH.text "frz"]
          ]

          , HH.div [HP.classes [HB.colSm6, (ClassName "file-buttons")]] [
                  HH.button
                    [ HP.classes [HB.buttonPrimary, HB.buttonSmall]
                      , HE.onClick $ HE.input_ OpenDatabase
                    ]
                    [ HH.text "Open " , HH.span [ HP.classes [HB.glyphiconOpen] ] [] ]
                  , HH.text " "
                  , HH.button
                    [ HP.classes [HB.buttonDefault, HB.buttonSmall]
                      , HE.onClick $ HE.input_ SaveDatabase
                    ]
                    [ HH.text "Save " , HH.span [ HP.classes [HB.glyphiconSave] ] [] ]
                  , HH.text " "
                  , HH.button
                    [ HP.classes [HB.buttonDefault, HB.buttonSmall]
                      , HE.onClick $ HE.input_ NewDatabase
                    ]
                    [ HH.text "New " , HH.span [ HP.classes [HB.glyphiconPlusSign] ] [] ]
                  , HH.text " "
                  , HH.button
                    [ HP.classes [HB.buttonDanger, HB.buttonSmall]
                      , HE.onClick $ HE.input_ RemoveDatabase
                    ]
                    [ HH.text "Delete " , HH.span [ HP.classes [HB.glyphiconRemoveSign] ] [] ]
          ]

        ],

        -- middle row
        HH.div [HP.classes [HB.row, (ClassName "middle-row")]] [
          HH.div [HP.classes [HB.colSm12]] [

          ]
        ],

        -- bottom row
        HH.div [HP.classes [HB.row, (ClassName "bottom-row")]] [
          HH.div [HP.classes [HB.colSm12]] [
            HH.text "Shoebox (c) 2017 - Frankfurt Haskell User Group"
          ]
        ]


      ] -- outer container

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        cmd <- H.gets _.word
        pr <- H.gets _.process
        s <- H.liftAff $ queryAPI pr cmd
        H.modify $ _ {gloss = s}
        pure next
    eval (OpenDatabase next) = do
--        H.liftAff $ log("open database pressed")
        pure next
    eval (SaveDatabase next) = do
--        H.liftAff $ log("open database pressed")
        pure next
    eval (NewDatabase next) = do
--        H.liftAff $ log("open database pressed")
        pure next
    eval (RemoveDatabase next) = do
--        H.liftAff $ log("open database pressed")
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
