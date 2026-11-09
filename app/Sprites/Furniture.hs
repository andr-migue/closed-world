module Sprites.Furniture (barrel, crate, urn) where

import Sprites.Sprite

barrel :: Sprite
barrel = sprite
  [ ".kkkk."
  , "kbnnbk"
  , "kSzSSk"
  , "kbnNbk"
  , "kbnnbk"
  , "kSzSSk"
  , "kbnnbk"
  , ".kkkk."
  ]

crate :: Sprite
crate = sprite
  [ "kkkkkkk"
  , "kBnnnBk"
  , "knBnBnk"
  , "knnBnnk"
  , "knBnBnk"
  , "kBnnnBk"
  , "kkkkkkk"
  ]

urn :: Sprite
urn = sprite
  [ ".kkk."
  , ".kOk."
  , "kooOk"
  , "koOOk"
  , "kOOOk"
  , ".kkk."
  ]
