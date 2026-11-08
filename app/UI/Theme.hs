module UI.Theme
  ( Name (..)
  , theme
  , aTitle, aTreasure, aTrap, aLine, aLineCurrent, aMessage, aKey, aCursor, aPicked
  , aHint, aSelected, aDim, aLabel, aValue
  , aFunctor, aAtom, aVariable, aPunct, aKeyword
  , aBarHigh, aBarMid, aBarLow, aBarEmpty
  , aGold, aCrimson, aArcane, aSteel, aEmerald
  ) where

import Brick
import Brick.Util (fg, on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.ProgressBar (progressCompleteAttr, progressIncompleteAttr)
import qualified Graphics.Vty as V
import UI.Palette

data Name = DungeonView
  deriving (Eq, Ord, Show)

aTitle, aTreasure, aTrap, aLine, aLineCurrent, aMessage, aKey, aCursor, aPicked,
  aHint, aSelected, aDim, aLabel, aValue,
  aFunctor, aAtom, aVariable, aPunct, aKeyword,
  aBarHigh, aBarMid, aBarLow, aBarEmpty,
  aGold, aCrimson, aArcane, aSteel, aEmerald :: AttrName
aTitle       = attrName "title"
aTreasure    = attrName "treasure"
aTrap        = attrName "trap"
aLine        = attrName "line"
aLineCurrent = attrName "lineCurrent"
aMessage     = attrName "message"
aKey         = attrName "key"
aCursor      = attrName "cursor"
aPicked      = attrName "picked"
aHint        = attrName "hint"
aSelected    = attrName "selected"
aDim         = attrName "dim"
aLabel       = attrName "label"
aValue       = attrName "value"
aFunctor     = attrName "functor"
aAtom        = attrName "atom"
aVariable    = attrName "variable"
aPunct       = attrName "punct"
aKeyword     = attrName "keyword"
aBarHigh     = attrName "barHigh"
aBarMid      = attrName "barMid"
aBarLow      = attrName "barLow"
aBarEmpty    = attrName "barEmpty"
aGold        = attrName "gold"
aCrimson     = attrName "crimson"
aArcane      = attrName "arcane"
aSteel       = attrName "steel"
aEmerald     = attrName "emerald"

ink :: RGB -> V.Attr
ink = fg . color

bold, italic :: V.Attr -> V.Attr
bold a   = a `V.withStyle` V.bold
italic a = a `V.withStyle` V.italic

on' :: RGB -> RGB -> V.Attr
on' f b = color f `on` color b

theme :: AttrMap
theme = attrMap V.defAttr
  [ (aTitle,       bold (ink gold))
  , (borderAttr,   ink wallShadow)
  , (aTreasure,    bold (ink gold))
  , (aTrap,        bold (ink crimson))
  , (aLine,        ink parchment)
  , (aLineCurrent, V.defAttr `V.withBackColor` color (58, 50, 86))
  , (aMessage,     italic (ink parchment))
  , (aKey,         on' outline gold)
  , (aCursor,      on' outline gold)
  , (aPicked,      on' parchment arcaneDeep)
  , (aHint,        italic (ink emerald))
  , (aSelected,    bold (on' outline gold))
  , (aDim,         ink mist)
  , (aLabel,       ink mist)
  , (aValue,       bold (ink parchment))
  , (aFunctor,     ink (126, 176, 255))
  , (aAtom,        ink emerald)
  , (aVariable,    bold (ink flame))
  , (aPunct,       ink mist)
  , (aKeyword,     bold (ink crimson))
  , (aBarHigh,     on' outline flame)
  , (aBarMid,      on' outline ember)
  , (aBarLow,      on' parchment crimson)
  , (aBarEmpty,    on' mist (40, 36, 52))
  , (aGold,        bold (ink gold))
  , (aCrimson,     bold (ink crimson))
  , (aArcane,      bold (ink arcane))
  , (aSteel,       ink steel)
  , (aEmerald,     bold (ink emerald))
  , (progressCompleteAttr,   on' outline flame)
  , (progressIncompleteAttr, on' mist (40, 36, 52))
  ]
