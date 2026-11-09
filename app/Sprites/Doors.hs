module Sprites.Doors (doorShut, doorSealed, doorShutNarrow, doorSealedNarrow) where

import Sprites.Sprite

doorShut :: Sprite
doorShut = sprite
  [ "kkkkk"
  , "knbnk"
  , "kbrbk"
  , "knbnk"
  ]

doorSealed :: Sprite
doorSealed = sprite
  [ "kkkkk"
  , "ksrsk"
  , "krprk"
  , "ksrsk"
  ]

doorShutNarrow :: Sprite
doorShutNarrow = sprite
  [ "kkk"
  , "krk"
  , "kbk"
  , "kkk"
  ]

doorSealedNarrow :: Sprite
doorSealedNarrow = sprite
  [ "kkk"
  , "kpk"
  , "krk"
  , "kkk"
  ]
