module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Data.Tuple
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Aff (Aff, delay)
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
import Data.Argonaut (decodeJson, jsonParser)

import Api (queryAPI)


data SelectedTab = TabInterL
                   | TabQuery
                   | TabTest

derive instance eqSelectedTab :: Eq SelectedTab

type State = { 

              -- child process, database backend
             process :: ChildProcess

              -- database info
              , dbFile :: String

              , currentDBs :: Array String

              -- status
              , statusLine :: String

              -- tab currently active
             , tab :: SelectedTab


              -- test tab commands
             , testCommand :: String
             , testResult :: String

             }


data ListSlot = ListSlot
derive instance eqListSlot :: Eq ListSlot
derive instance ordListSlot :: Ord ListSlot

data Query a = OpenDatabase a
             | SaveDatabase a
             | NewDatabase a
             | RemoveDatabase a
             | SetTab SelectedTab a
             | UpdateTestCommand String a
             | RunTestCommand a  
             | MessageStatus String a
             | HandleList HB.ListMessage a
             | Initialize a

ui :: forall eff. ChildProcess -> H.Component HH.HTML Query Unit Void (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
ui cpIn = H.lifecycleParentComponent
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState = { 
                   process : cpIn
                   , dbFile : "frz"
                   , currentDBs : []
                   , statusLine : ""
                   , tab : TabTest
                   , testCommand : ""
                   , testResult : ""
                   }

    render :: State -> H.ParentHTML Query HB.ListQuery ListSlot (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
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
              HH.h3_ [ HH.small_ [ HH.text "selected database: " ], HH.text st.dbFile]
            ]

            , HH.div [HP.classes [HB.colSm6, (ClassName "file-buttons")]] [
                    -- Open Button
                    HH.button
                      [ HP.classes [HB.buttonPrimary, HB.buttonSmall]
                        , HB.dataToggle "modal", HB.dataTarget "#modalIdOpenDB"
                      ]
                      [ HH.text "Open " , HH.span [ HP.classes [HB.glyphiconOpen] ] [] ]
                    , HH.text " "
                    -- Save Button
                    , HH.button
                      [ HP.classes [HB.buttonDefault, HB.buttonSmall]
                        , HE.onClick $ HE.input_ SaveDatabase
                      ]
                      [ HH.text "Save " , HH.span [ HP.classes [HB.glyphiconSave] ] [] ]
                    , HH.text " "
                    -- Save As Button
                    , HH.button
                      [ HP.classes [HB.buttonDefault, HB.buttonSmall]
                        , HE.onClick $ HE.input_ SaveDatabase
                      ]
                      [ HH.text "Save As " , HH.span [ HP.classes [HB.glyphiconSave] ] [] ]
                    , HH.text " "
                    -- New Button
                    , HH.button
                      [ HP.classes [HB.buttonDefault, HB.buttonSmall]
                        , HE.onClick $ HE.input_ NewDatabase
                      ]
                      [ HH.text "New " , HH.span [ HP.classes [HB.glyphiconPlusSign] ] [] ]
                    , HH.text " "
                    -- Delete Button
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
              if st.statusLine == ""
                then HH.text "Shoebox (c) 2017 - Frankfurt Haskell User Group"
                else HH.div_ [HH.b_ [HH.text "Status: "], HH.text st.statusLine]
            ]
          ]


        , HB.modal "modalIdOpenDB" "Open Database" 
            ( HH.slot ListSlot (HB.myListGroup ["one", "two"]) unit (HE.input HandleList) ) 
            (    
              HH.div [] [
                HH.button
                  [ HP.class_ HB.buttonDefault, HB.dataDismiss "modal"] 
                  [ HH.text "Cancel" ] 
                , HH.button
                  [ HP.class_ HB.buttonPrimary, HB.dataDismiss "modal", HE.onClick $ HE.input_ OpenDatabase]  
                  [ HH.text "Open" ] 
                ]
            )
        ] -- outer container


    eval :: Query ~> H.ParentDSL State Query HB.ListQuery ListSlot Void (Aff (exception :: EXCEPTION, cp::CHILD_PROCESS, buffer::BUFFER  | eff))
    eval message = 
      case message of
        (Initialize next) -> do
          pr <- H.gets _.process
          s <- H.liftAff $ queryAPI pr "current-dbs"
          let a = jsonParser s
          case a of
            Left _ -> pure next
            Right a' -> do
              let ar = decodeJson a'
              case ar of
                Left _ -> pure next
                Right ar' -> do
                  H.modify $ _ {currentDBs = decodeJson ar'}
                  pure next

        (OpenDatabase next) -> pure next 

        (SaveDatabase next) -> do
          pr <- H.gets _.process
          s <- H.liftAff $ queryAPI pr "save-db"
          H.modify $ _ {statusLine = s}
          H.liftAff $ delay (Milliseconds 3000.0) 
          H.modify $ _ {statusLine = ""}
          pure next

        (NewDatabase next) -> pure next
        (RemoveDatabase next) -> pure next

        (SetTab newT next) -> do
          H.modify $ _ {tab = newT}
          pure next

        (UpdateTestCommand newc next) -> do
          H.modify $ _ {testCommand = newc}
          pure next

        (RunTestCommand next) -> do
          cmd <- H.gets _.testCommand
          pr <- H.gets _.process
          s <- H.liftAff $ queryAPI pr cmd
          H.modify $ _ {testResult = s}
          pure next
        
        (HandleList (HB.Selected msg) next) -> do
          pure next

        (MessageStatus msg next) -> do
          H.modify $ _ {statusLine = msg}
          H.liftAff $ delay (Milliseconds 3000.0) 
          H.modify $ _ {statusLine = ""}
          pure next

