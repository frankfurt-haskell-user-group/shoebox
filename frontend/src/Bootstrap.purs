module Bootstrap where

import Halogen.HTML (ClassName(..))
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (attr)
import Data.Maybe (Maybe(..))

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








