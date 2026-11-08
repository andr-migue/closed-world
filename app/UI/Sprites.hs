module UI.Sprites
  ( adventurer, adventurerWalk, ghost, chest, skull, rubble, entrance, portal
  , flame1, flame2, torchTall, torchOut, star, starEmpty
  , levelIcon, starsImage
  ) where

import qualified Graphics.Vty as V
import UI.Palette
import UI.Pixel

sprite :: [String] -> Sprite
sprite = map (map pixel)

pixel :: Char -> Maybe RGB
pixel c = lookup c inks

inks :: [(Char, RGB)]
inks =
  [ ('k', outline), ('g', gold), ('G', goldDeep), ('y', goldLight)
  , ('f', flame), ('F', flameLight), ('e', ember)
  , ('r', crimson), ('R', crimsonDeep)
  , ('v', arcane), ('V', arcaneDeep), ('c', spectral)
  , ('w', bone), ('b', wood), ('B', woodDeep), ('s', steel), ('S', steelDeep)
  , ('p', cloak), ('P', cloakDeep), ('h', skin), ('m', emerald), ('M', emeraldDeep)
  ]

adventurer, ghost, chest, skull, rubble, entrance, portal :: Sprite
adventurer = sprite
  [ "...PP..F"
  , "..PPPP.f"
  , "..PhhP.B"
  , "...hh..B"
  , "..pppppB"
  , ".pPpppP."
  , "..pp.pp."
  , "..kk.kk."
  ]
ghost = sprite
  [ "..cccc.."
  , ".cvvvvc."
  , ".ckvvkc."
  , ".cvvvvc."
  , ".cvVVvc."
  , ".cvvvvc."
  , ".c.cc.c."
  , "........"
  ]

adventurerWalk :: Sprite
adventurerWalk = sprite
  [ "...PP..F"
  , "..PPPP.f"
  , "..PhhP.B"
  , "...hh..B"
  , "..pppppB"
  , ".pPpppP."
  , ".pp...pp"
  , ".kk...kk"
  ]
chest = sprite
  [ "........"
  , ".GggggG."
  , "GgyyyygG"
  , "BbbmmbbB"
  , "BbbyybbB"
  , "BbbbbbbB"
  , "BBBBBBBB"
  , "........"
  ]
skull = sprite
  [ "..wwww.."
  , ".wwwwww."
  , ".wkwwkw."
  , ".wkwwkw."
  , ".wwwwww."
  , "..wkkw.."
  , "..w.ww.."
  , "...rR..."
  ]
rubble = sprite
  [ "........"
  , "........"
  , "...SS..."
  , "..SsS.S."
  , ".SssSSsS"
  , "SsSSsSSS"
  , "SSSSSSSS"
  , "........"
  ]
entrance = sprite
  [ "..ssss.."
  , ".sSkkSs."
  , "sSkkkkSs"
  , "sSkFFkSs"
  , "sSkffkSs"
  , "sSkkkkSs"
  , "SSSSSSSS"
  , "........"
  ]
portal = sprite
  [ "...cc..."
  , "..cvvc.."
  , ".cvccvc."
  , "cvc..cvc"
  , ".cvccvc."
  , "..cvvc.."
  , "...cc..."
  , "........"
  ]

flame1, flame2, torchTall, torchOut, star, starEmpty :: Sprite
flame1 = sprite
  [ "..f..."
  , ".fFf.."
  , ".fFFf."
  , "fFyFf."
  , ".fFf.."
  , "..B..."
  , "..B..."
  , "..b..."
  ]
flame2 = sprite
  [ "...f.."
  , "..fFf."
  , ".fFFf."
  , ".fFyFf"
  , "..fFf."
  , "...B.."
  , "...B.."
  , "...b.."
  ]
torchTall = sprite
  [ "..ff.."
  , ".fFFf."
  , "fFyyFf"
  , ".fFFf."
  , "SsssS."
  , ".SBS.."
  , "..B..."
  , "..B..."
  , "..B..."
  , "..b..."
  ]
torchOut = sprite
  [ "........"
  , "...SS..."
  , "..S..S.."
  , "...SS..."
  , "..BBBB.."
  , "...BB..."
  , "...BB..."
  , "...bb..."
  ]
star = sprite
  [ "...y..."
  , "..yGy.."
  , "yyyGyyy"
  , ".gGGGg."
  , ".gG.Gg."
  , "gG...Gg"
  ]
starEmpty = sprite
  [ "...S..."
  , "..S.S.."
  , "SSS.SSS"
  , ".S...S."
  , ".S.S.S."
  , "SS...SS"
  ]

key, axe, gate, door, lock, corridor :: Sprite
key = sprite
  [ "........"
  , ".yyy...."
  , "y..Gy..."
  , "y..Gyyyy"
  , ".yyy..Gy"
  , "......G."
  , "........"
  , "........"
  ]
axe = sprite
  [ ".ss....."
  , "sSss...."
  , "sSSsB..."
  , ".sSsB..."
  , "...BB..."
  , "....B..."
  , "....B..."
  , ".....b.."
  ]
gate = sprite
  [ "SSSSSSSS"
  , "SsSsSsSs"
  , "SsSsSsSs"
  , "SSSSSSSS"
  , "SsSsSsSs"
  , "SsSsSsSs"
  , "rsrsrsrs"
  , "........"
  ]
door = sprite
  [ "..BBBB.."
  , ".BbbbbB."
  , "BbbbbbbB"
  , "BbbbbgbB"
  , "BbbbbbbB"
  , "BbbbbbbB"
  , "BbbbbbbB"
  , "SSSSSSSS"
  ]
lock = sprite
  [ "..SSSS.."
  , ".S....S."
  , ".S....S."
  , "gggggggg"
  , "gGGkkGGg"
  , "gGGkkGGg"
  , "gGGGGGGg"
  , "gggggggg"
  ]
corridor = sprite
  [ "sSSSSSSs"
  , "S.kkkk.S"
  , "S.k..k.S"
  , "S.k..k.S"
  , "S.kffk.S"
  , "S.kffk.S"
  , "S......S"
  , "SSSSSSSS"
  ]

levelIcon :: Int -> Sprite
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

starsImage :: Int -> V.Image
starsImage earned = V.horizCat (concat [[spriteImage (pick i), V.char V.defAttr ' '] | i <- [1 .. 3]])
  where pick i = if i <= earned then star else starEmpty
