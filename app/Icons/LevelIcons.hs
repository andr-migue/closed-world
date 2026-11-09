module Icons.LevelIcons (levelIcon) where

import Icons.Characters (ghost)
import Icons.Inks (icon)
import Icons.Places (entrance, portal)
import Pixels.Icon (Icon)

levelIcon :: Int -> Icon
levelIcon n = case n of
  1 -> corridor
  2 -> gate
  3 -> ghost
  4 -> key
  5 -> door
  6 -> portal
  7 -> lock
  8 -> axe
  _ -> entrance

corridor :: Icon
corridor = icon
  [ "sSSSSSSs"
  , "S.kkkk.S"
  , "S.k..k.S"
  , "S.k..k.S"
  , "S.kffk.S"
  , "S.kffk.S"
  , "S......S"
  , "SSSSSSSS"
  ]

gate :: Icon
gate = icon
  [ "SSSSSSSS"
  , "SsSsSsSs"
  , "SsSsSsSs"
  , "SSSSSSSS"
  , "SsSsSsSs"
  , "SsSsSsSs"
  , "rsrsrsrs"
  , "........"
  ]

key :: Icon
key = icon
  [ "........"
  , ".yyy...."
  , "y..Gy..."
  , "y..Gyyyy"
  , ".yyy..Gy"
  , "......G."
  , "........"
  , "........"
  ]

door :: Icon
door = icon
  [ "..BBBB.."
  , ".BbbbbB."
  , "BbbbbbbB"
  , "BbbbbgbB"
  , "BbbbbbbB"
  , "BbbbbbbB"
  , "BbbbbbbB"
  , "SSSSSSSS"
  ]

lock :: Icon
lock = icon
  [ "..SSSS.."
  , ".S....S."
  , ".S....S."
  , "gggggggg"
  , "gGGkkGGg"
  , "gGGkkGGg"
  , "gGGGGGGg"
  , "gggggggg"
  ]

axe :: Icon
axe = icon
  [ ".ss....."
  , "sSss...."
  , "sSSsB..."
  , ".sSsB..."
  , "...BB..."
  , "....B..."
  , "....B..."
  , ".....b.."
  ]
