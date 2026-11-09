module Sprites.Remains (bones, skullSmall) where

import Sprites.Sprite

bones :: Sprite
bones = sprite
  [ ".w...wx"
  , "wwxxxw."
  , ".x...xw"
  ]

skullSmall :: Sprite
skullSmall = sprite
  [ ".www."
  , "wkwkw"
  , "wwwww"
  , ".wXw."
  ]
