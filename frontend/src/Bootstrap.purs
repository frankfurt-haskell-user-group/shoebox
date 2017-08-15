module Bootstrap where

import Data.List 
import Data.Tuple
import Data.Maybe (Maybe(..))

import Prelude

import Halogen.HTML (ClassName(..))
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (attr)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA

container :: ClassName
container = ClassName "container"

containerFluid :: ClassName
containerFluid = ClassName "container-fluid"

row :: ClassName
row = ClassName "row"

colSm1 :: ClassName
colSm1 = ClassName "col-sm-1"

colSm2 :: ClassName
colSm2 = ClassName "col-sm-2"

colSm3 :: ClassName
colSm3 = ClassName "col-sm-3"

colSm4 :: ClassName
colSm4 = ClassName "col-sm-4"

colSm5 :: ClassName
colSm5 = ClassName "col-sm-5"

colSm6 :: ClassName
colSm6 = ClassName "col-sm-6"

colSm7 :: ClassName
colSm7 = ClassName "col-sm-7"

colSm8 :: ClassName
colSm8 = ClassName "col-sm-8"

colSm9 :: ClassName
colSm9 = ClassName "col-sm-9"

colSm10 :: ClassName
colSm10 = ClassName "col-sm-10"

colSm11 :: ClassName
colSm11 = ClassName "col-sm-11"

colSm12 :: ClassName
colSm12 = ClassName "col-sm-12"

lead :: ClassName
lead = ClassName "lead"

textLeft :: ClassName
textLeft = ClassName "text-left"

textRight :: ClassName
textRight = ClassName "text-right"

textCenter :: ClassName
textCenter = ClassName "text-center"

textJustify :: ClassName
textJustify = ClassName "text-justify"

textNowrap :: ClassName
textNowrap = ClassName "text-nowrap"

buttonDefault :: ClassName
buttonDefault = ClassName "btn btn-default"

buttonPrimary :: ClassName
buttonPrimary = ClassName "btn btn-primary"

buttonSuccess :: ClassName
buttonSuccess = ClassName "btn btn-success"

buttonInfo :: ClassName
buttonInfo = ClassName "btn btn-info"

buttonWarning :: ClassName
buttonWarning = ClassName "btn btn-warning"

buttonDanger :: ClassName
buttonDanger = ClassName "btn btn-danger"

buttonLink :: ClassName
buttonLink = ClassName "btn btn-link"

buttonSmall :: ClassName
buttonSmall = ClassName "btn-sm"

buttonExtraSmall :: ClassName
buttonExtraSmall = ClassName "btn-xs"

buttonLarge :: ClassName
buttonLarge = ClassName "btn-lg"


glyphiconSend :: ClassName
glyphiconSend = ClassName "glyphicon glyphicon-send"

glyphiconOpen :: ClassName
glyphiconOpen = ClassName "glyphicon glyphicon-open"

glyphiconSave :: ClassName
glyphiconSave = ClassName "glyphicon glyphicon-save"

glyphiconPlusSign :: ClassName
glyphiconPlusSign = ClassName "glyphicon glyphicon-plus-sign"

glyphiconRemoveSign :: ClassName
glyphiconRemoveSign = ClassName "glyphicon glyphicon-remove-sign"


navTabs :: ClassName
navTabs = ClassName "nav nav-tabs"

navTabActive :: ClassName
navTabActive = ClassName "active"





{-
Button Dropdown

<div class="dropdown">
  <button class="btn btn-default dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
    Dropdown
    <span class="caret"></span>
  </button>
  <ul class="dropdown-menu" aria-labelledby="dropdownMenu1">
    <li><a href="#">Action</a></li>
    <li><a href="#">Another action</a></li>
    <li><a href="#">Something else here</a></li>
    <li role="separator" class="divider"></li>
    <li><a href="#">Separated link</a></li>
  </ul>
</div>

-}

dropdown :: ClassName
dropdown = ClassName "dropdown"

dropup :: ClassName
dropup = ClassName "dropup"

buttonDropdownToggle :: ClassName
buttonDropdownToggle = ClassName "btn btn-default dropdown-toggle"

dropdownMenu :: ClassName
dropdownMenu = ClassName "dropdown-menu"

caret :: ClassName
caret = ClassName "caret"

formGroup :: ClassName
formGroup = ClassName "form-group"

formFor = attr (AttrName "for")

formControl :: ClassName
formControl = ClassName "form-control"
{-

      HH.div [HP.class_ HB.row] [
        HH.div [HP.class_ HB.colSm3] [   -- menu section
          HH.div [HP.class_ (ClassName "btn-group-vertical")] [
           HH.button
              [ HP.disabled $ st.word == mempty
              , HE.onClick $ HE.input_ QueryWord
              ]
              [ HH.text "Send " , HH.span [ HP.class_ (ClassName "glyphicon glyphicon-send")] [] ]
          , HH.button
              [ HP.disabled $ st.word == mempty
              , HE.onClick $ HE.input_ QueryWord
              ]
              [ HH.text "Send " , HH.span [ HP.class_ (ClassName "glyphicon glyphicon-send")] [] ]
          ]
        ],

        HH.div [HP.class_ HB.colSm9] [   -- content section


        HH.div [HP.class_ (ClassName "dropdown")] [
          HH.button [ HP.class_ (ClassName "btn btn-secondary dropdown-toggle"), 
                      HP.attr (AttrName "type") "button", 
                      HP.attr (AttrName "id") "dropdownMenuButton" ,
                      HP.attr (AttrName "data-toggle") "dropdown", 
                      HP.attr (AttrName "aria-haspopup") "true", 
                      HP.attr (AttrName "aria-expanded") "false"
                      ] 
                      [
                        HH.text "drop this" 
                      ]
          , HH.ul [HP.class_ HB.dropdownMenu, HP.attr (AttrName "aria-labelledby") "dropdownMenuButton"] [
              HH.li [] [ HH.a [HP.class_ (ClassName "dropdown-item"), HP.attr (AttrName "href") "#"] [HH.text "wow"] ] ,
              HH.li [] [ HH.a [HP.class_ (ClassName "dropdown-item"), HP.attr (AttrName "href") "#"] [HH.text "wow2"] ] 
              ]
          ]

        , HH.p [][]
      
        , HH.input
            [ HP.value st.word
            , HE.onValueInput $ HE.input UpdateWord
            ]
        , HH.button
            [ HP.disabled $ st.word == mempty
            , HE.onClick $ HE.input_ QueryWord
            ]
            [ HH.text "Send " , HH.span [ HP.class_ (ClassName "glyphicon glyphicon-send")] [] ]
        , HH.p_ [ HH.text st.segmentation ]
        , HH.p_ [ HH.text st.gloss ]

        ]
      ],

      -- bottom row 
      HH.div [HP.class_ (ClassName "row")] [
        HH.div [HP.class_ (ClassName "col-sm-12")] [
          HH.text "Shoebox (c) 2017 - Frankfurt Haskell User Group"
        ]
      ]



-}



-- Modal Dialogs
modalDialog :: ClassName
modalDialog = ClassName "modal-dialog"

modalContent :: ClassName
modalContent = ClassName "modal-content"

modalHeader :: ClassName
modalHeader = ClassName "modal-header"

modalFooter :: ClassName
modalFooter = ClassName "modal-footer"

modalFade :: ClassName
modalFade = ClassName "modal fade"

modalTitle :: ClassName
modalTitle = ClassName "modal-title"

close :: ClassName
close = ClassName "close"


dataToggle = attr (AttrName "data-toggle") 
dataTarget = attr (AttrName "data-target") 
dataDismiss = attr (AttrName "data-dismiss") 

role = attr (AttrName "role") 

modal modalId myTitle myContent myFooter = 
    HH.div [HP.class_ modalFade, HP.id_ modalId, role "dialog"] [
        HH.div [HP.class_ modalDialog] [
            HH.div [HP.class_ modalHeader] [
                HH.button [HP.class_ close, dataDismiss "modal"] [HH.text "×"]
                , HH.h4 [HP.class_ modalTitle] [HH.text myTitle]
            ]
            , HH.div [HP.class_ modalContent] [
                myContent
            ]
            , HH.div [HP.class_ modalFooter] [
                myFooter
            ]
        ]
    ]



-- Alerts

alertSuccess :: ClassName
alertSuccess = ClassName "alert alert-success"

alertInfo :: ClassName
alertInfo = ClassName "alert alert-info"

alertWarning :: ClassName
alertWarning = ClassName "alert alert-warning"

alertDanger :: ClassName
alertDanger = ClassName "alert alert-danger"

-- list groups

listGroup :: ClassName
listGroup = ClassName "list-group"

listGroupItem :: ClassName
listGroupItem = ClassName "list-group-item list-group-item-action"

type_ = attr (AttrName "type")


-- Tab-List component

type ListState = {
    labels :: Array String,
    selected :: Maybe String
} 

data ListQuery a
    = ListSelect String a
    | ListUnselect a
    | ListSelected (Maybe String -> a)

type ListInput = Unit

data ListMessage = Selected (Maybe String)

myListGroup :: forall m. Array String -> H.Component HH.HTML ListQuery ListInput ListMessage m
myListGroup al = 
    H.component 
        {
            initialState : const initialState,
            render,
            eval,
            receiver: const Nothing
        }
        where

        initialState :: ListState
        initialState = { labels : al, selected : Nothing }

        render :: ListState -> H.ComponentHTML ListQuery
        render state = HH.div [HP.class_ listGroup] [HH.div []  
                            ((\label -> let 
                                selectedText l = HH.button [HP.classes [navTabActive, listGroupItem], type_ "button", HE.onClick (HE.input_ (ListSelect l))] [HH.text l] 
                                unselectedText l = HH.button [HP.class_ listGroupItem, type_ "button", HE.onClick (HE.input_ (ListSelect l))] [HH.text l] 
                                in case state.selected of
                                    Nothing -> unselectedText label
                                    Just s -> if s == label
                                                then selectedText label
                                                else unselectedText label
                                ) <$> state.labels)
                            ]

        eval :: ListQuery ~> H.ComponentDSL ListState ListQuery ListMessage m
        eval = case _ of
            ListSelect label next -> do                
                H.modify (_ {selected = Just label})
                pure next
            ListUnselect next -> do                
                H.modify (_ {selected = Nothing})
                pure next
            ListSelected reply -> do
                state <- H.get
                pure (reply state.selected)

