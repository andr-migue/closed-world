module Icons.Places (entrance, chest, skull, rubble, portal) where

import Icons.Inks (icon)
import Pixels.Icon (Icon)

entrance :: Icon
entrance = icon
  [ "..ssss.."
  , ".sSkkSs."
  , "sSkkkkSs"
  , "sSkFFkSs"
  , "sSkffkSs"
  , "sSkkkkSs"
  , "SSSSSSSS"
  , "........"
  ]

chest :: Icon
chest = icon
  [ "........"
  , ".GggggG."
  , "GgyyyygG"
  , "BbbmmbbB"
  , "BbbyybbB"
  , "BbbbbbbB"
  , "BBBBBBBB"
  , "........"
  ]

skull :: Icon
skull = icon
  [ "..wwww.."
  , ".wwwwww."
  , ".wkwwkw."
  , ".wkwwkw."
  , ".wwwwww."
  , "..wkkw.."
  , "..w.ww.."
  , "...rR..."
  ]

rubble :: Icon
rubble = icon
  [ "........"
  , "........"
  , "...SS..."
  , "..SsS.S."
  , ".SssSSsS"
  , "SsSSsSSS"
  , "SSSSSSSS"
  , "........"
  ]

portal :: Icon
portal = icon
  [ "...cc..."
  , "..cvvc.."
  , ".cvccvc."
  , "cvc..cvc"
  , ".cvccvc."
  , "..cvvc.."
  , "...cc..."
  , "........"
  ]
