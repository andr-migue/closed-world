module Sprites.Sprite
  ( Sprite
  , sprite
  , spriteWidth
  , spriteHeight
  , mirrored
  , stamp
  , stampGlow
  , stampBlend
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import Pixels.Canvas
import Pixels.Color
import Sprites.Inks (inkOf)
import Style.Colors (RGB)

data Sprite = Sprite
  { spriteWidth  :: Int
  , spriteHeight :: Int
  , spritePixels :: U.Vector Int
  }

sprite :: [String] -> Sprite
sprite rows = Sprite w (length rows) (U.fromList (concatMap line rows))
  where
    w = maximum (0 : map length rows)
    line r = map (maybe transparent pack . inkOf) (take w (r ++ repeat '.'))

transparent :: Int
transparent = -1

mirrored :: Sprite -> Sprite
mirrored s = s { spritePixels = U.generate (spriteWidth s * spriteHeight s) flipped }
  where
    flipped i =
      let (y, x) = i `divMod` spriteWidth s
      in spritePixels s U.! (y * spriteWidth s + spriteWidth s - 1 - x)

stamp :: Canvas s -> Tone -> Int -> Int -> Sprite -> ST s ()
stamp cv = stampWith (plot cv)

stampGlow :: Canvas s -> Tone -> Int -> Int -> Sprite -> ST s ()
stampGlow cv = stampWith (glow cv)

stampBlend :: Canvas s -> Double -> Tone -> Int -> Int -> Sprite -> ST s ()
stampBlend cv a = stampWith (blend cv a)

stampWith :: (Int -> Int -> RGB -> ST s ()) -> Tone -> Int -> Int -> Sprite -> ST s ()
stampWith put tone x y s =
  forM_ [0 .. spriteHeight s - 1] $ \j -> forM_ [0 .. spriteWidth s - 1] $ \i -> do
    let p = spritePixels s U.! (j * spriteWidth s + i)
    when (p /= transparent) (put (x + i) (y + j) (tone (unpack p)))
