module App (
  app
) where

import Prelude
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.ChildProcess (CHILD_PROCESS)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Node, Element, ElementId(..), documentToNonElementParentNode)
import DOM.Node.Node (nodeName)
import Data.Maybe (fromJust, Maybe (..))
import Partial.Unsafe (unsafePartial)
import React (Ref, readRef, writeRef, ReactElement, ReactClass, createFactory, createClass, writeState, readState, spec, getProps)
import ReactDOM (render, refToNode)
import Data.Array (find)
import Data.Nullable (toNullable)

import Data.Tuple (Tuple(..), snd)
import Data.Nullable (Nullable(..), toNullable)

import TabInterL (tabInterL)
import TabQuery (tabQuery)
import TabTest (tabTest)
import DbSelect (dbSelect)

import CommandDispatcher (dispatcher, CommandDispatcher)

-- tab component

tabs :: forall props. Array (Tuple String ReactElement) -> ReactClass props
tabs lis = createClass $ spec  { currentTab: "Query" } \ctx -> do
  st <- readState ctx
  pure $ D.div' [
    D.ul [ P.className "nav nav-tabs" ] (map (\(Tuple t' e) -> liTab ctx st.currentTab t') lis )
    , unsafePartial ((snd <<< fromJust) (find (\ (Tuple t' el) -> t' == st.currentTab) lis))
    ]
  where
    liTab ctx t tab = if (t == tab)
      then D.li [ P.className "active" ] [ D.a [ P.href "#" ] [ D.text tab ] ]
      else D.li [] [ D.a [ P.href "#" , P.onClick (clickF ctx tab) ] [ D.text tab ] ]
    clickF ctx tab = \_ -> writeState ctx { currentTab: tab }

statusLine :: String -> ReactElement
statusLine s = if (s == "")
  then D.div' [ D.text "Shoebox (c) 2017 - Frankfurt Haskell User Group" ]
  else D.div' [ D.text "Status: "
                , D.b [] [D.text s]
	      ]

appComponent :: forall props. ReactClass { dispatcher::CommandDispatcher | props }
appComponent = createClass $ spec unit \ctx -> do
  props <- getProps ctx
  pure $ D.div [P.className "container"] [
    createFactory dbSelect {}
    , D.div [P.className "row middle-row"] [
        D.div [P.className "col-sm-12"] [
           createFactory (tabs [
             Tuple "Query" (createFactory tabQuery { dispatcher: props.dispatcher })
             , Tuple "Inter-L" (createFactory tabInterL { dispatcher: props.dispatcher })
             , Tuple "Test" (createFactory tabTest { dispatcher: props.dispatcher })
             ] ) props
        ]
      ]
    , D.div [P.className "row bottom-row"] [
        D.div [P.className "col-sm-12"] [
          statusLine ""
        ]
      ]
    ]

app :: forall eff. Eff (dom :: DOM, console::CONSOLE, cp::CHILD_PROCESS, exception::EXCEPTION | eff) Unit
app = do
  d <- dispatcher
  void (elm' >>= render (ui d))
  where
  ui :: CommandDispatcher -> ReactElement
  ui d = D.div' [ createFactory appComponent { dispatcher: d } ]
 
  elm' :: forall eff. Eff (dom :: DOM | eff) Element
  elm' = do
    win <- window
    doc <- document win
    elm <- getElementById (ElementId "root") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    pure $ unsafePartial (fromJust elm)

 
