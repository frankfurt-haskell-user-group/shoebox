module TabQuery (
   tabQuery
   ) where

import Prelude
import React (ReactClass, spec, createClass)
import React.DOM.Dynamic as D

tabQuery :: forall props. ReactClass { | props }
tabQuery = createClass $ spec unit \ctx -> do
  pure $ D.div' [
     D.text "This should be Query Tab Head"
     ]
