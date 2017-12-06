module TabInterL (
   tabInterL
   ) where

import Prelude
import React (ReactClass, spec, createClass)
import React.DOM.Dynamic as D

tabInterL :: forall props. ReactClass { | props }
tabInterL = createClass $ spec unit \ctx -> do
  pure $ D.div' [
     D.text "This should be Inter-L Tab Head"
     ]
