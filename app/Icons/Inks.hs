module Icons.Inks (icon) where

import Pixels.Icon (Icon)
import Style.Colors

icon :: [String] -> Icon
icon = map (map (`lookup` inks))

inks :: [(Char, RGB)]
inks =
  [ ('k', outline), ('g', gold), ('G', goldDeep), ('y', goldLight)
  , ('f', flame), ('F', flameLight), ('e', ember)
  , ('r', crimson), ('R', crimsonDeep)
  , ('v', arcane), ('V', arcaneDeep), ('c', spectral)
  , ('w', bone), ('b', wood), ('B', woodDeep), ('s', steel), ('S', steelDeep)
  , ('p', cloak), ('P', cloakDeep), ('h', skin), ('m', emerald), ('M', emeraldDeep)
  ]
