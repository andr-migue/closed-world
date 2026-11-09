module Icons.Torch (flame1, flame2, torchOut) where

import Icons.Inks (icon)
import Pixels.Icon (Icon)

flame1 :: Icon
flame1 = icon
  [ "..f..."
  , ".fFf.."
  , ".fFFf."
  , "fFyFf."
  , ".fFf.."
  , "..B..."
  , "..B..."
  , "..b..."
  ]

flame2 :: Icon
flame2 = icon
  [ "...f.."
  , "..fFf."
  , ".fFFf."
  , ".fFyFf"
  , "..fFf."
  , "...B.."
  , "...B.."
  , "...b.."
  ]

torchOut :: Icon
torchOut = icon
  [ "........"
  , "...SS..."
  , "..S..S.."
  , "...SS..."
  , "..BBBB.."
  , "...BB..."
  , "...BB..."
  , "...bb..."
  ]
