module UI.Palette
  ( RGB
  , color
  , dim
  , night, floorStone, floorWarm, floorGhost
  , wallLight, wallShadow, wallGold, wallGoldShadow, wallGhost, wallGhostShadow
  , gold, goldDeep, goldLight, ember, flame, flameLight
  , crimson, crimsonDeep, emerald, emeraldDeep
  , arcane, arcaneDeep, spectral
  , parchment, mist, outline
  , wood, woodDeep, steel, steelDeep
  , cloak, cloakDeep, skin, bone
  , panelBg
  ) where

import qualified Graphics.Vty as V

type RGB = (Int, Int, Int)

color :: RGB -> V.Color
color (r, g, b) = V.linearColor r g b

dim :: RGB -> RGB
dim (r, g, b) = (mix r 18, mix g 20, mix b 32)
  where mix c bg = bg + (c - bg) * 2 `div` 5

panelBg :: RGB
panelBg = (22, 20, 34)

night, floorStone, floorWarm, floorGhost :: RGB
night      = (18, 20, 32)
floorStone = (32, 36, 56)
floorWarm  = (52, 44, 52)
floorGhost = (38, 30, 64)

wallLight, wallShadow, wallGold, wallGoldShadow, wallGhost, wallGhostShadow :: RGB
wallLight       = (118, 130, 176)
wallShadow      = (78, 88, 128)
wallGold        = (238, 184, 96)
wallGoldShadow  = (176, 122, 58)
wallGhost       = (150, 120, 236)
wallGhostShadow = (98, 76, 172)

gold, goldDeep, goldLight, ember, flame, flameLight :: RGB
gold       = (255, 196, 92)
goldDeep   = (212, 138, 48)
goldLight  = (255, 234, 168)
ember      = (222, 92, 52)
flame      = (255, 140, 52)
flameLight = (255, 222, 112)

crimson, crimsonDeep, emerald, emeraldDeep :: RGB
crimson     = (214, 58, 82)
crimsonDeep = (132, 30, 56)
emerald     = (72, 210, 156)
emeraldDeep = (30, 132, 104)

arcane, arcaneDeep, spectral :: RGB
arcane     = (166, 124, 246)
arcaneDeep = (100, 72, 178)
spectral   = (134, 228, 244)

parchment, mist, outline :: RGB
parchment = (236, 226, 204)
mist      = (122, 128, 156)
outline   = (22, 20, 34)

wood, woodDeep, steel, steelDeep :: RGB
wood      = (150, 96, 60)
woodDeep  = (96, 58, 42)
steel     = (168, 178, 204)
steelDeep = (98, 106, 136)

cloak, cloakDeep, skin, bone :: RGB
cloak     = (58, 120, 168)
cloakDeep = (36, 74, 122)
skin      = (240, 196, 160)
bone      = (236, 230, 214)
