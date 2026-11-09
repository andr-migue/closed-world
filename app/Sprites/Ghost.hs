module Sprites.Ghost (ghostFrames) where

import Sprites.Sprite

ghostFrames :: [Sprite]
ghostFrames = map sprite
  [ [ "..cccc.."
    , ".cCCCCc."
    , "cCvvvvCc"
    , "cvkvvkvc"
    , "cvvvvvvc"
    , "cvvVVvvc"
    , "cvvvvvvc"
    , "cv.vv.vc"
    , ".c..c..c"
    ]
  , [ "..cccc.."
    , ".cCCCCc."
    , "cCvvvvCc"
    , "cvkvvkvc"
    , "cvvvvvvc"
    , "cvvVVvvc"
    , "cvvvvvvc"
    , "c.vv.vvc"
    , "c..c..c."
    ]
  ]
