module UI.Theme
  ( Name (..)
  , theme
  , aTitle, aRoom, aCurrent, aGhost, aTreasure, aTrap, aDeadEnd
  , aConnector, aLine, aLineCurrent, aMessage, aKey, aCursor, aPicked, aHint, aSelected, aDim
  ) where

import Brick
import Brick.Util (fg, on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.ProgressBar (progressCompleteAttr, progressIncompleteAttr)
import qualified Graphics.Vty as V

data Name = DungeonView
  deriving (Eq, Ord, Show)

aTitle, aRoom, aCurrent, aGhost, aTreasure, aTrap, aDeadEnd,
  aConnector, aLine, aLineCurrent, aMessage, aKey, aCursor, aPicked, aHint, aSelected, aDim :: AttrName
aTitle       = attrName "title"
aRoom        = attrName "room"
aCurrent     = attrName "current"
aGhost       = attrName "ghost"
aTreasure    = attrName "treasure"
aTrap        = attrName "trap"
aDeadEnd     = attrName "deadEnd"
aConnector   = attrName "connector"
aLine        = attrName "line"
aLineCurrent = attrName "lineCurrent"
aMessage     = attrName "message"
aKey         = attrName "key"
aCursor      = attrName "cursor"
aPicked      = attrName "picked"
aHint        = attrName "hint"
aSelected    = attrName "selected"
aDim         = attrName "dim"

rgb :: Int -> Int -> Int -> V.Color
rgb = V.linearColor

theme :: AttrMap
theme = attrMap V.defAttr
  [ (aTitle,       fg (rgb 255 200 80) `V.withStyle` V.bold)
  , (borderAttr,   fg (rgb 110 110 140))
  , (aRoom,        fg (rgb 200 200 215))
  , (aCurrent,     fg (rgb 255 220 120) `V.withStyle` V.bold)
  , (aGhost,       fg (rgb 160 190 255))
  , (aTreasure,    fg (rgb 255 210 60) `V.withStyle` V.bold)
  , (aTrap,        fg (rgb 255 80 80) `V.withStyle` V.bold)
  , (aDeadEnd,     fg (rgb 120 120 135))
  , (aConnector,   fg (rgb 200 170 110))
  , (aLine,        fg (rgb 190 190 200))
  , (aLineCurrent, rgb 255 255 255 `on` rgb 60 55 90)
  , (aMessage,     fg (rgb 230 210 170) `V.withStyle` V.italic)
  , (aKey,         rgb 20 20 30 `on` rgb 200 170 110)
  , (aCursor,      rgb 20 20 30 `on` rgb 255 200 80)
  , (aPicked,      rgb 255 255 255 `on` rgb 70 90 160)
  , (aHint,        fg (rgb 160 220 170) `V.withStyle` V.italic)
  , (aSelected,    rgb 20 20 30 `on` rgb 255 200 80 `V.withStyle` V.bold)
  , (aDim,         fg (rgb 120 120 140))
  , (progressCompleteAttr,   rgb 20 20 20 `on` rgb 255 150 30)
  , (progressIncompleteAttr, rgb 200 200 200 `on` rgb 60 50 50)
  ]
