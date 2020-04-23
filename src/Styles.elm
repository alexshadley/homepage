module Styles exposing (..)

import Css exposing (..)

mainHeader : List Style
mainHeader =
  [ fontWeight bold
  , textAlign center
  , marginTop (em 1)
  , marginBottom (em 2)
  , fontSize (em 3.5)
  ]

wrapper : List Style
wrapper =
  [ marginLeft auto
  , marginRight auto
  , maxWidth (px 1080)
  , fontFamilies [ "Helvetica" ]
  ]

masonry : List Style
masonry =
  [ property "column-count" "3"
  , property "column-gap" "2em"
  ]

item : List Style
item =
  [ display inlineBlock
  , margin3 (px 0) (px 0) (em 2)
  , width (pct 100)
  , color (rgb 0 0 0)
  ]

itemHeader : List Style
itemHeader =
  [ fontWeight bold ]

itemDescription : List Style
itemDescription =
  [ ]