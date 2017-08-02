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

data SelectedTab = TabInterL
                   | TabQuery
                   | TabTest

derive instance eqSelectedTab :: Eq SelectedTab

type State = { word         :: Word
             , segmentation :: String
             , gloss        :: String
             , process :: ChildProcess
             , tab :: SelectedTab
             , testCommand :: String
             , testResult :: String
             }

data Query a = UpdateWord Word a
             | QueryWord a
             | OpenDatabase a
             | SaveDatabase a
             | NewDatabase a
             | RemoveDatabase a
             | SetTab SelectedTab a
             | UpdateTestCommand String a
             | RunTestCommand a  

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
                   , tab : TabTest
                   , testCommand : ""
                   , testResult : ""
                   }

    render :: State -> H.ComponentHTML Query
    render st =  

      let activeProp t1 t2 = if t1 == t2 
            then [HP.classes [HB.navTabActive]] 
            else []
      in

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

          ]

          -- middle row
          , HH.div [HP.classes [HB.row, (ClassName "middle-row")]] [
              HH.div [HP.classes [HB.colSm12]] [

                -- nav tabs
                HH.ul [HP.classes [HB.navTabs]] [
                  -- li items are the content ones
                  HH.li (activeProp TabInterL st.tab) [
                    HH.a [HP.href "#", HE.onClick (HE.input_ (SetTab TabInterL))] [
                      HH.text "Inter-L"
                      ]
                  ]
                  -- li items are the content ones
                  , HH.li (activeProp TabQuery st.tab) [
                    HH.a [HP.href "#", HE.onClick (HE.input_ (SetTab TabQuery))] [
                      HH.text "Query"
                      ]
                  ]
                  -- li items are the content ones
                  , HH.li (activeProp TabTest st.tab) [
                    HH.a [HP.href "#", HE.onClick (HE.input_ (SetTab TabTest))] [
                      HH.text "Test"
                      ]
                  ]
                ]

            -- content
            , case st.tab of
              TabInterL -> 
                -- interlinearisation pane
                HH.h3_ [ HH.small_ [ HH.text "Interlinearization Panel"]]

              TabQuery -> 
                -- query pane
                HH.h3_ [ HH.small_ [ HH.text "Query Panel"]]

              TabTest -> HH.div [] [ 
                -- test pane
                HH.h3_ [ HH.small_ [ HH.text "Test Panel"]]
                , HH.text "you can try the commands: 'current-db', 'save-db', and 'available-dbs'.", HH.p_ []
                -- input form, cmd
                , HH.div [HP.classes [HB.formGroup]] [
                  HH.label [HB.formFor "command"] [HH.text "Command:"]
                  , HH.input [HE.onValueChange (HE.input UpdateTestCommand), HP.classes [HB.formControl], HP.id_ "command"] 
                ]
                , HH.p_ []
                , HH.button
                  [ HP.classes [HB.buttonPrimary]
                    , HE.onClick $ HE.input_ RunTestCommand
                  ]
                  [ HH.text "Execute" ] 
                , HH.p_ [], HH.text "Test Result:", HH.p_ [], HH.text st.testResult
                ]


            ]
          ]

          -- bottom row
          , HH.div [HP.classes [HB.row, (ClassName "bottom-row")]] [
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
    eval (SetTab newT next) = do
        H.modify $ _ {tab = newT}
--        H.liftAff $ log("open database pressed")
        pure next
    eval (UpdateTestCommand newC next) = do
        H.modify $ _ {testCommand = newC}
--        H.liftAff $ log("open database pressed")
        pure next
    eval (RunTestCommand next) = do
        cmd <- H.gets _.testCommand
        pr <- H.gets _.process
        s <- H.liftAff $ queryAPI pr cmd
        H.modify $ _ {testResult = s}
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
