module DbSelect (
   dbSelect
   ) where

import Prelude
import React (createFactory, ReactClass, ReactElement, spec, createClass, getProps, readState, writeState)
import React (ReactRefs, ReactState, ReactProps, Read, Write, EventHandlerContext)
import React.DOM.Dynamic as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import DbStore as S


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

modalDialog :: String -> String -> ReactElement -> ReactElement -> ReactElement
modalDialog _id title content footer = D.div [P.className "modal fade", P.role "dialog", P._id _id]
  [
    D.div [P.className "modal-dialog"]
      [
        D.div [P.className "modal-content"]
          [
            D.div [P.className "modal-header"]
              [
                D.button [P.className "close", P._data { dismiss: "modal" }] [D.text "x"]
                , D.h4 [P.className "modal-title"] [D.text title]
              ]
           ,D.div [P.className "modal-body"]
              [
                content
              ]
           ,D.div [P.className "modal-footer"]
              [
                footer
              ]
          ]
      ]
  ]

listGroup :: forall props props' eff state. ReactClass { items :: Array String, notifyChange :: String -> EventHandlerContext eff props state Unit | props' }
listGroup = createClass $ spec { activeItem: "" } \ ctx -> do
  p <- getProps ctx
  s <- readState ctx
  pure $ D.div [ P.className "list-group"]
                   (map (\i -> D.button (
                         let cb = \e -> do
                                          _ <- writeState ctx { activeItem: i }
                                          p.notifyChange i 
                                          pure unit
                         in if s.activeItem == i
                              then
                                [P.className "list-goup-item active", P._type "button"]
                              else
                                [P.className "list-goup-item", P._type "button", P.onClick cb]
                        ) [D.text i] ) p.items)
 
dbSelect :: forall eff props. ReactClass { store :: S.DbStore eff | props }
dbSelect = createClass $ spec unit \ctx -> do
  p <- getProps ctx
  pure $ D.div' [
          D.div [P.className "row top-row"] [
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
         , modalDialog "modalIdOpenDB" "Open Database"
               (D.div' [ createFactory listGroup { items: let (S.DbStore s) = p.store in s.dbs, notifyChange: (\_ -> pure unit) } ])
               (D.div' [])
         , modalDialog "modalIdSaveAsDB" "Save Database as"
               (D.div' [])
               (D.div' [])
         , modalDialog "modalIdDeleteDB" "Delete Database"
               (D.div' [])
               (D.div' [])
         , modalDialog "modalIdNewDB" "Create Database"
               (D.div' [])
               (D.div' [])
         ]
