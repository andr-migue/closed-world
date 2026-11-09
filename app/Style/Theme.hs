module Style.Theme
  ( theme
  , aTitle, aTrap, aLine, aLineCurrent, aMessage, aKey, aKeyLabel, aCursor, aPicked
  , aHint, aSelected, aDim, aLabel, aValue
  , aFunctor, aAtom, aVariable, aPunct, aKeyword
  , aGold, aCrimson, aArcane, aSteel, aEmerald, aFlame
  , aPanel, aPanelEdge, aPanelTitle, aPanelInfo, aPanelTrack
  , aHeader, aBrand, aChip, aChipOff, aChipCut, aChipLabel, aNotice
  , aGutter, aGutterCurrent, aRowCursor, aEntryCurrent
  ) where

import Brick
import qualified Graphics.Vty as V
import Style.Colors

aTitle, aTrap, aLine, aLineCurrent, aMessage, aKey, aKeyLabel, aCursor, aPicked,
  aHint, aSelected, aDim, aLabel, aValue,
  aFunctor, aAtom, aVariable, aPunct, aKeyword,
  aGold, aCrimson, aArcane, aSteel, aEmerald, aFlame,
  aPanel, aPanelEdge, aPanelTitle, aPanelInfo, aPanelTrack,
  aHeader, aBrand, aChip, aChipOff, aChipCut, aChipLabel, aNotice,
  aGutter, aGutterCurrent, aRowCursor, aEntryCurrent :: AttrName
aTitle         = attrName "title"
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
aChipLabel     = attrName "chipLabel"
aNotice        = attrName "notice"
aGutter        = attrName "gutter"
aGutterCurrent = attrName "gutterCurrent"
aRowCursor     = attrName "rowCursor"
aEntryCurrent  = attrName "entryCurrent"

theme :: AttrMap
theme = attrMap (textOn parchment appBg)
  [ (aTitle,         bold (ink gold))
  , (aTrap,          bold (ink crimson))
  , (aLine,          ink parchment)
  , (aLineCurrent,   background (52, 42, 70))
  , (aRowCursor,     background (32, 30, 50))
  , (aEntryCurrent,  background (40, 34, 60))
  , (aMessage,       italic (ink parchment))
  , (aKey,           bold (textOn outline gold))
  , (aKeyLabel,      ink (176, 172, 196))
  , (aCursor,        bold (textOn outline gold))
  , (aPicked,        bold (textOn parchment arcaneDeep))
  , (aHint,          italic (ink emerald))
  , (aSelected,      bold (textOn outline gold))
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
  , (aPanel,         textOn parchment panelBg)
  , (aPanelEdge,     ink (74, 66, 110))
  , (aPanelTitle,    bold (ink parchment))
  , (aPanelInfo,     ink mist)
  , (aPanelTrack,    ink (48, 44, 72))
  , (aHeader,        textOn parchment (28, 24, 42))
  , (aBrand,         bold (textOn outline gold))
  , (aChip,          bold (textOn parchment (58, 50, 86)))
  , (aChipOff,       textOn mist (34, 32, 50))
  , (aChipCut,       bold (textOn parchment crimsonDeep))
  , (aChipLabel,     textOn (176, 172, 196) (28, 24, 42))
  , (aNotice,        bold (textOn parchment crimsonDeep))
  , (aGutter,        ink (84, 82, 112))
  , (aGutterCurrent, bold (ink gold))
  ]

ink :: RGB -> V.Attr
ink c = V.defAttr `V.withForeColor` color c

background :: RGB -> V.Attr
background c = V.defAttr `V.withBackColor` color c

textOn :: RGB -> RGB -> V.Attr
textOn f b = ink f `V.withBackColor` color b

bold, italic :: V.Attr -> V.Attr
bold a   = a `V.withStyle` V.bold
italic a = a `V.withStyle` V.italic
