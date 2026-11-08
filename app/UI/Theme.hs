module UI.Theme
  ( Name (..)
  , theme
  , appBg, panelBg
  , aTitle, aTreasure, aTrap, aLine, aLineCurrent, aMessage, aKey, aKeyLabel, aCursor, aPicked
  , aHint, aSelected, aDim, aLabel, aValue
  , aFunctor, aAtom, aVariable, aPunct, aKeyword
  , aGold, aCrimson, aArcane, aSteel, aEmerald, aFlame
  , aPanel, aPanelEdge, aPanelTitle, aPanelInfo, aPanelTrack
  , aHeader, aBrand, aChip, aChipOff, aChipCut, aGutter, aGutterCurrent, aRowCursor
  , aEntryCurrent, aChipLabel, aNotice
  ) where

import Brick
import Brick.Widgets.Border (borderAttr)
import qualified Graphics.Vty as V
import UI.Palette

data Name = DungeonView | ProgramView | QueryView | ChronicleView
  | CacheHeader | CacheFooter | CacheProgram | CacheQuery | CacheChronicle
  deriving (Eq, Ord, Show)

appBg :: RGB
appBg = (13, 12, 21)

aTitle, aTreasure, aTrap, aLine, aLineCurrent, aMessage, aKey, aKeyLabel, aCursor, aPicked,
  aHint, aSelected, aDim, aLabel, aValue,
  aFunctor, aAtom, aVariable, aPunct, aKeyword,
  aGold, aCrimson, aArcane, aSteel, aEmerald, aFlame,
  aPanel, aPanelEdge, aPanelTitle, aPanelInfo, aPanelTrack,
  aHeader, aBrand, aChip, aChipOff, aChipCut, aGutter, aGutterCurrent, aRowCursor,
  aEntryCurrent, aChipLabel, aNotice :: AttrName
aTitle         = attrName "title"
aTreasure      = attrName "treasure"
aTrap          = attrName "trap"
aLine          = attrName "line"
aLineCurrent   = attrName "lineCurrent"
aMessage       = attrName "message"
aKey           = attrName "key"
aKeyLabel      = attrName "keyLabel"
aCursor        = attrName "cursor"
aPicked        = attrName "picked"
aHint          = attrName "hint"
aSelected      = attrName "selected"
aDim           = attrName "dim"
aLabel         = attrName "label"
aValue         = attrName "value"
aFunctor       = attrName "functor"
aAtom          = attrName "atom"
aVariable      = attrName "variable"
aPunct         = attrName "punct"
aKeyword       = attrName "keyword"
aGold          = attrName "gold"
aCrimson       = attrName "crimson"
aArcane        = attrName "arcane"
aSteel         = attrName "steel"
aEmerald       = attrName "emerald"
aFlame         = attrName "flame"
aPanel         = attrName "panel"
aPanelEdge     = attrName "panelEdge"
aPanelTitle    = attrName "panelTitle"
aPanelInfo     = attrName "panelInfo"
aPanelTrack    = attrName "panelTrack"
aHeader        = attrName "header"
aBrand         = attrName "brand"
aChip          = attrName "chip"
aChipOff       = attrName "chipOff"
aChipCut       = attrName "chipCut"
aGutter        = attrName "gutter"
aGutterCurrent = attrName "gutterCurrent"
aRowCursor     = attrName "rowCursor"
aEntryCurrent  = attrName "entryCurrent"
aChipLabel     = attrName "chipLabel"
aNotice        = attrName "notice"

ink :: RGB -> V.Attr
ink = fg . color

bold, italic :: V.Attr -> V.Attr
bold a   = a `V.withStyle` V.bold
italic a = a `V.withStyle` V.italic

on' :: RGB -> RGB -> V.Attr
on' f b = color f `on` color b

theme :: AttrMap
theme = attrMap (on' parchment appBg)
  [ (aTitle,         bold (ink gold))
  , (borderAttr,     ink (70, 64, 104))
  , (aTreasure,      bold (ink gold))
  , (aTrap,          bold (ink crimson))
  , (aLine,          ink parchment)
  , (aLineCurrent,   V.defAttr `V.withBackColor` color (52, 42, 70))
  , (aRowCursor,     V.defAttr `V.withBackColor` color (32, 30, 50))
  , (aEntryCurrent,  V.defAttr `V.withBackColor` color (40, 34, 60))
  , (aMessage,       italic (ink parchment))
  , (aKey,           bold (on' outline gold))
  , (aKeyLabel,      ink (176, 172, 196))
  , (aCursor,        bold (on' outline gold))
  , (aPicked,        bold (on' parchment arcaneDeep))
  , (aHint,          italic (ink emerald))
  , (aSelected,      bold (on' outline gold))
  , (aDim,           ink mist)
  , (aLabel,         ink mist)
  , (aValue,         bold (ink parchment))
  , (aFunctor,       ink (126, 176, 255))
  , (aAtom,          ink emerald)
  , (aVariable,      bold (ink flame))
  , (aPunct,         ink mist)
  , (aKeyword,       bold (ink crimson))
  , (aGold,          bold (ink gold))
  , (aCrimson,       bold (ink crimson))
  , (aArcane,        bold (ink arcane))
  , (aSteel,         ink steel)
  , (aEmerald,       bold (ink emerald))
  , (aFlame,         bold (ink flame))
  , (aPanel,         on' parchment panelBg)
  , (aPanelEdge,     ink (74, 66, 110))
  , (aPanelTitle,    bold (ink parchment))
  , (aPanelInfo,     ink mist)
  , (aPanelTrack,    ink (48, 44, 72))
  , (aHeader,        on' parchment (28, 24, 42))
  , (aBrand,         bold (on' outline gold))
  , (aChip,          bold (on' parchment (58, 50, 86)))
  , (aChipOff,       on' mist (34, 32, 50))
  , (aChipCut,       bold (on' parchment crimsonDeep))
  , (aChipLabel,     on' (176, 172, 196) (28, 24, 42))
  , (aNotice,        bold (on' parchment crimsonDeep))
  , (aGutter,        ink (84, 82, 112))
  , (aGutterCurrent, bold (ink gold))
  ]
