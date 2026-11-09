module Style.Colors
  ( RGB
  , color
  , appBg, panelBg
  , stone
  , gold, goldDeep, goldLight, ember, flame, flameLight
  , crimson, crimsonDeep, emerald, emeraldDeep
  , arcane, arcaneDeep, spectral
  , parchment, mist, outline
  , wood, woodDeep, steel, steelDeep, wallLight, wallShadow
  , cloak, cloakDeep, skin, bone
  ) where

import qualified Graphics.Vty as V

type RGB = (Int, Int, Int)

color :: RGB -> V.Color
color (r, g, b) = V.linearColor r g b

appBg, panelBg :: RGB
appBg   = (13, 12, 21)
panelBg = (22, 20, 34)

stone :: Int -> RGB
stone n = case n of
  1 -> (40, 42, 62)
  2 -> (58, 62, 88)
  3 -> (82, 88, 118)
  4 -> (110, 118, 152)
  _ -> (146, 156, 190)

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

wood, woodDeep, steel, steelDeep, wallLight, wallShadow :: RGB
wood       = (150, 96, 60)
woodDeep   = (96, 58, 42)
steel      = (168, 178, 204)
steelDeep  = (98, 106, 136)
wallLight  = (118, 130, 176)
wallShadow = (78, 88, 128)

cloak, cloakDeep, skin, bone :: RGB
cloak     = (58, 120, 168)
cloakDeep = (36, 74, 122)
skin      = (240, 196, 160)
bone      = (236, 230, 214)
