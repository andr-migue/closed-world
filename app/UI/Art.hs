module UI.Art
  ( Art
  , artW
  , artH
  , art
  , stamp
  , stampGlow
  , stampBlend
  , mirrored
  , heroDown, heroUp, heroSide
  , spirit
  , sconce, sconceFlames, sconceCold
  , banner, cobweb, bones, skullSmall, barrel, crate, urn, mushrooms
  , chestShut, chestOpen
  , doorShut, doorSealed, doorShutNarrow, doorSealedNarrow
  , stoneTone
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import UI.Canvas
import UI.Palette (RGB)

data Art = Art
  { artW  :: Int
  , artH  :: Int
  , artPx :: U.Vector Int
  }

art :: [String] -> Art
art rows = Art w (length rows) (U.fromList (concatMap line rows))
  where
    w = maximum (0 : map length rows)
    line r = map ink (take w (r ++ repeat '.'))
    ink c = maybe (-1) pack (lookup c inks)

mirrored :: Art -> Art
mirrored a = a { artPx = U.generate (artW a * artH a) flip' }
  where flip' i = let (y, x) = i `divMod` artW a in artPx a U.! (y * artW a + artW a - 1 - x)

stampWith :: (Int -> Int -> RGB -> ST s ()) -> Tone -> Int -> Int -> Art -> ST s ()
stampWith put tone x y a =
  forM_ [0 .. artH a - 1] $ \j -> forM_ [0 .. artW a - 1] $ \i -> do
    let p = artPx a U.! (j * artW a + i)
    when (p >= 0) (put (x + i) (y + j) (tone (unpack p)))

stamp :: Canvas s -> Tone -> Int -> Int -> Art -> ST s ()
stamp cv = stampWith (plot cv)

stampGlow :: Canvas s -> Tone -> Int -> Int -> Art -> ST s ()
stampGlow cv = stampWith (glow cv)

stampBlend :: Canvas s -> Double -> Tone -> Int -> Int -> Art -> ST s ()
stampBlend cv a = stampWith (blend cv a)

stoneTone :: Int -> RGB
stoneTone n = case n of
  1 -> (40, 42, 62)
  2 -> (58, 62, 88)
  3 -> (82, 88, 118)
  4 -> (110, 118, 152)
  _ -> (146, 156, 190)

inks :: [(Char, RGB)]
inks =
  [ ('k', (26, 20, 36)), ('K', (12, 9, 18))
  , ('1', stoneTone 1), ('2', stoneTone 2), ('3', stoneTone 3), ('4', stoneTone 4), ('5', stoneTone 5)
  , ('B', (78, 46, 34)), ('b', (122, 78, 50)), ('n', (166, 112, 70)), ('N', (204, 152, 100))
  , ('G', (168, 100, 36)), ('g', (232, 168, 60)), ('y', (255, 222, 130)), ('Y', (255, 248, 214))
  , ('e', (196, 58, 36)), ('f', (250, 132, 46)), ('F', (255, 196, 86)), ('W', (255, 246, 200))
  , ('R', (104, 22, 42)), ('r', (182, 42, 64)), ('p', (236, 104, 116))
  , ('V', (64, 42, 128)), ('v', (130, 92, 222)), ('c', (190, 168, 255)), ('C', (156, 236, 250))
  , ('M', (30, 86, 60)), ('m', (58, 142, 92)), ('l', (124, 198, 112))
  , ('t', (110, 240, 214)), ('T', (40, 156, 152))
  , ('h', (246, 200, 160)), ('H', (204, 142, 112))
  , ('a', (116, 70, 44)), ('A', (70, 40, 32))
  , ('u', (34, 58, 112)), ('U', (56, 98, 170)), ('i', (108, 160, 226))
  , ('o', (180, 74, 48)), ('O', (120, 46, 38))
  , ('w', (238, 230, 208)), ('x', (182, 172, 152)), ('X', (124, 116, 104))
  , ('s', (152, 164, 190)), ('S', (94, 102, 130)), ('z', (216, 224, 242))
  ]

heroDown, heroUp, heroSide :: [Art]
heroDown = map art
  [ [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUaAAaUk", "kUhhhhUk", "kUkhhkUk"
    , ".kHhhHk.", ".kOrrOk.", "kuUUUUuk", "khuUUuhk", ".kuBBuk.", "..kk.kk." ]
  , [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUaAAaUk", "kUhhhhUk", "kUkhhkUk"
    , ".kHhhHk.", ".kOrrOk.", "kuUUUUuk", "khuUUuhk", ".kuBBuk.", "..kk...." ]
  , [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUaAAaUk", "kUhhhhUk", "kUkhhkUk"
    , ".kHhhHk.", ".kOrrOk.", "kuUUUUuk", "khuUUuhk", ".kuBBuk.", "....kk.." ]
  ]
heroUp = map art
  [ [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUUiiUUk", "kUUUUUUk", "kuUUUUuk"
    , ".kuUUuk.", ".kOrrOk.", "kuUuuUuk", "khuuuuhk", ".kuBBuk.", "..kk.kk." ]
  , [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUUiiUUk", "kUUUUUUk", "kuUUUUuk"
    , ".kuUUuk.", ".kOrrOk.", "kuUuuUuk", "khuuuuhk", ".kuBBuk.", "..kk...." ]
  , [ "..kkkk..", ".kUUUUk.", "kUiiiiUk", "kUUiiUUk", "kUUUUUUk", "kuUUUUuk"
    , ".kuUUuk.", ".kOrrOk.", "kuUuuUuk", "khuuuuhk", ".kuBBuk.", "....kk.." ]
  ]
heroSide = map art
  [ [ "..kkkk..", ".kUUiik.", "kUUiiiUk", "kUUUaaAk", "kUUUhhhk", "kUUUhkhk"
    , ".kUUHhk.", ".kOrrrk.", ".kuUUuk.", ".kuUhuk.", ".kuBBk..", "..kkk..." ]
  , [ "..kkkk..", ".kUUiik.", "kUUiiiUk", "kUUUaaAk", "kUUUhhhk", "kUUUhkhk"
    , ".kUUHhk.", ".kOrrrk.", ".kuUUuk.", ".kuUhuk.", ".kuBBuk.", ".kk..kk." ]
  , [ "..kkkk..", ".kUUiik.", "kUUiiiUk", "kUUUaaAk", "kUUUhhhk", "kUUUhkhk"
    , ".kUUHhk.", ".kOrrrk.", ".kuUUuk.", ".kuUhuk.", ".kuBBk..", "..kkk..." ]
  ]

spirit :: [Art]
spirit = map art
  [ [ "..cccc..", ".cCCCCc.", "cCvvvvCc", "cvkvvkvc", "cvvvvvvc", "cvvVVvvc", "cvvvvvvc", "cv.vv.vc", ".c..c..c" ]
  , [ "..cccc..", ".cCCCCc.", "cCvvvvCc", "cvkvvkvc", "cvvvvvvc", "cvvVVvvc", "cvvvvvvc", "c.vv.vvc", "c..c..c." ]
  ]

sconce :: Art
sconce = art [ "SzS", ".S." ]

sconceFlames :: [Art]
sconceFlames = map art
  [ [ ".f.", "fFf", "FWF" ]
  , [ "f..", ".Ff", "FWF" ]
  , [ "..f", "fF.", "FWF" ]
  , [ ".f.", "Fff", "FWF" ]
  ]

sconceCold :: Art
sconceCold = art [ "...", ".X.", "XkX" ]

banner :: Art
banner = art
  [ "SzzzzzS", ".RrrrR.", ".RrgrR.", ".RgygR.", ".RrgrR.", ".RrrrR.", ".RrrrR.", ".Rr.rR.", ".R...R." ]

cobweb :: Art
cobweb = art [ "xxxxxx", "xx.x..", "x.xx..", "xx....", "x.x...", "x....." ]

bones :: Art
bones = art [ ".w...wx", "wwxxxw.", ".x...xw" ]

skullSmall :: Art
skullSmall = art [ ".www.", "wkwkw", "wwwww", ".wXw." ]

barrel :: Art
barrel = art [ ".kkkk.", "kbnnbk", "kSzSSk", "kbnNbk", "kbnnbk", "kSzSSk", "kbnnbk", ".kkkk." ]

crate :: Art
crate = art [ "kkkkkkk", "kBnnnBk", "knBnBnk", "knnBnnk", "knBnBnk", "kBnnnBk", "kkkkkkk" ]

urn :: Art
urn = art [ ".kkk.", ".kOk.", "kooOk", "koOOk", "kOOOk", ".kkk." ]

mushrooms :: Art
mushrooms = art [ ".tt...", "tTTt.t", ".w..tT", ".w...w" ]

chestShut :: Art
chestShut = art
  [ ".kkkkkkkk.", "kbnnnnnnbk", "kGggggggGk", "kbnnyynnbk", "kBbbGGbbBk", "kBbbbbbbBk", "kGggggggGk", ".kkkkkkkk." ]

chestOpen :: Art
chestOpen = art
  [ ".kkkkkkkk.", "kBbbbbbbBk", "kbBBBBBBbk", "kyYyyYyyYk", "kGggggggGk", "kbnnyynnbk", "kBbbGGbbBk", "kBbbbbbbBk", ".kkkkkkkk." ]

doorShut, doorSealed, doorShutNarrow, doorSealedNarrow :: Art
doorShut         = art [ "kkkkk", "knbnk", "kbrbk", "knbnk" ]
doorSealed       = art [ "kkkkk", "ksrsk", "krprk", "ksrsk" ]
doorShutNarrow   = art [ "kkk", "krk", "kbk", "kkk" ]
doorSealedNarrow = art [ "kkk", "kpk", "krk", "kkk" ]
