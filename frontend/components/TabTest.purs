module TabTest (
   tabTest
   ) where

import Prelude
import React (ReactClass, spec, createClass)
import React.DOM.Dynamic as D

tabTest :: forall props. ReactClass { | props }
tabTest = createClass $ spec unit \ctx -> do
  pure $ D.div' [
     D.text "This should be Test Tab Head"
     ]
