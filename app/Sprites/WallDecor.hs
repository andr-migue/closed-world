module Sprites.WallDecor (wallBanner, cobweb) where

import Sprites.Sprite

wallBanner :: Sprite
wallBanner = sprite
  [ "SzzzzzS"
  , ".RrrrR."
  , ".RrgrR."
  , ".RgygR."
  , ".RrgrR."
  , ".RrrrR."
  , ".RrrrR."
  , ".Rr.rR."
  , ".R...R."
  ]

cobweb :: Sprite
cobweb = sprite
  [ "xxxxxx"
  , "xx.x.."
  , "x.xx.."
  , "xx...."
  , "x.x..."
  , "x....."
  ]
