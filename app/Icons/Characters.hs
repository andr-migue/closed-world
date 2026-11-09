module Icons.Characters (adventurer, ghost) where

import Icons.Inks (icon)
import Pixels.Icon (Icon)

adventurer :: Icon
adventurer = icon
  [ "...PP..F"
  , "..PPPP.f"
  , "..PhhP.B"
  , "...hh..B"
  , "..pppppB"
  , ".pPpppP."
  , "..pp.pp."
  , "..kk.kk."
  ]

ghost :: Icon
ghost = icon
  [ "..cccc.."
  , ".cvvvvc."
  , ".ckvvkc."
  , ".cvvvvc."
  , ".cvVVvc."
  , ".cvvvvc."
  , ".c.cc.c."
  , "........"
  ]
