module DbSelect (
   dbSelect
   ) where

import Prelude
import React (ReactClass, ReactElement, spec, createClass)
import React.DOM.Dynamic as D
import React.DOM.Props as P

actionButton :: String -> String -> String -> ReactElement
actionButton bt text glyph = D.button [
  P.className ("btn btn-" <> bt <> " btn-sm")
  ]
  [
    D.text text
    , D.span [P.className ("glyphicon glyphicon-" <> glyph)][]
  ]

modalButton :: String -> String -> String -> String -> ReactElement
modalButton bt target text glyph = D.button [
  P.className ("btn btn-" <> bt <> " btn-sm")
  , P._data { toggle: "modal", target: target }
  ]
  [
    D.text text
    , D.span [P.className ("glyphicon glyphicon-" <> glyph)][]
  ]

dbSelect :: forall props. ReactClass { | props }
dbSelect = createClass $ spec unit \ctx -> do
  pure $ D.div [P.className "row top-row"] [
            D.div [P.className "col-sm-6"] [
              D.h3' [
                      D.small' [
                        D.text "selected database: "
                      ]
                      , D.text "frz"
                    ]]

            , D.div [P.className "col-sm-6 file-buttons"] [
               modalButton "primary" "#modalIdOpenDB" "Open " "open"
               , D.text " "
               , actionButton "default" "Save " "save"
               , D.text " "
               , modalButton "default" "#modalIdSaveAsDB" "Save As " "save"
               , D.text " "
               , modalButton "default" "#modalIdNewDB" "New " "plus-sign"
               , D.text " "
               , modalButton "danger" "#modalIdDeleteDB" "Delete " "remove-sign"
            ]
          ]
