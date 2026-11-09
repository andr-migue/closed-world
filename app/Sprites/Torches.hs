module Sprites.Torches (sconce, sconceFlames, sconceCold) where

import Sprites.Sprite

sconce :: Sprite
sconce = sprite
  [ "SzS"
  , ".S."
  ]

sconceFlames :: [Sprite]
sconceFlames = map sprite
  [ [ ".f."
    , "fFf"
    , "FWF"
    ]
  , [ "f.."
    , ".Ff"
    , "FWF"
    ]
  , [ "..f"
    , "fF."
    , "FWF"
    ]
  , [ ".f."
    , "Fff"
    , "FWF"
    ]
  ]

sconceCold :: Sprite
sconceCold = sprite
  [ "..."
  , ".X."
  , "XkX"
  ]
