{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module UI.Canvas
  ( Canvas
  , Pixel
  , Tone
  , Light (..)
  , Label (..)
  , Frame (..)
  , pack
  , unpack
  , paint
  , plot
  , glow
  , blend
  , rect
  , glowRect
  , blendRect
  , darken
  , darkenRect
  , readPixel
  , inView
  , canvasImage
  , shadePixels
  , mixRGB
  , scaleRGB
  , luma
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vty as V
import Graphics.Vty.Image.Internal (Image (HorizText))
import UI.Palette (RGB, color)

type Pixel = Int

type Tone = RGB -> RGB

data Canvas s = Canvas
  { cvW    :: !Int
  , cvH    :: !Int
  , cvX    :: !Int
  , cvY    :: !Int
  , cvBase :: M.MVector s Int
  , cvLit  :: M.MVector s Bool
  }

data Light = Light
  { lightX      :: !Double
  , lightY      :: !Double
  , lightRadius :: !Double
  , lightColor  :: !(Double, Double, Double)
  , lightPower  :: !Double
  }

data Label = Label
  { labelX    :: !Int
  , labelY    :: !Int
  , labelText :: T.Text
  , labelInk  :: RGB
  , labelBold :: Bool
  }

data Frame = Frame
  { frameW       :: Int
  , frameH       :: Int
  , frameX       :: Int
  , frameY       :: Int
  , frameAmbient :: Double
  , frameLights  :: [Light]
  , frameLabels  :: [Label]
  , frameVignette :: Bool
  }

pack :: RGB -> Pixel
pack (r, g, b) = (clamp8 r `shiftL` 16) + (clamp8 g `shiftL` 8) + clamp8 b

unpack :: Pixel -> RGB
unpack p = (p `shiftR` 16 .&. 255, p `shiftR` 8 .&. 255, p .&. 255)

clamp8 :: Int -> Int
clamp8 = max 0 . min 255

paint :: Frame -> RGB -> (forall s. Canvas s -> ST s ()) -> (U.Vector Int, U.Vector Bool)
paint fr background draw = runST $ do
  base <- M.replicate (frameW fr * frameH fr) (pack background)
  lit  <- M.replicate (frameW fr * frameH fr) False
  let cv = Canvas (frameW fr) (frameH fr) (frameX fr) (frameY fr) base lit
  draw cv
  b <- U.freeze base
  l <- U.freeze lit
  pure (b, l)

index :: Canvas s -> Int -> Int -> Maybe Int
index cv x y
  | lx < 0 || ly < 0 || lx >= cvW cv || ly >= cvH cv = Nothing
  | otherwise = Just (ly * cvW cv + lx)
  where
    lx = x - cvX cv
    ly = y - cvY cv

inView :: Canvas s -> Int -> Int -> Int -> Int -> Bool
inView cv x y w h =
  x + w > cvX cv && y + h > cvY cv && x < cvX cv + cvW cv && y < cvY cv + cvH cv

plot :: Canvas s -> Int -> Int -> RGB -> ST s ()
plot cv x y c = forM_ (index cv x y) $ \i -> do
  M.unsafeWrite (cvBase cv) i (pack c)
  M.unsafeWrite (cvLit cv) i False

glow :: Canvas s -> Int -> Int -> RGB -> ST s ()
glow cv x y c = forM_ (index cv x y) $ \i -> do
  M.unsafeWrite (cvBase cv) i (pack c)
  M.unsafeWrite (cvLit cv) i True

blend :: Canvas s -> Double -> Int -> Int -> RGB -> ST s ()
blend cv a x y c = forM_ (index cv x y) $ \i -> do
  old <- M.unsafeRead (cvBase cv) i
  M.unsafeWrite (cvBase cv) i (pack (mixRGB a (unpack old) c))

darken :: Canvas s -> Double -> Int -> Int -> ST s ()
darken cv f x y = forM_ (index cv x y) $ \i -> do
  old <- M.unsafeRead (cvBase cv) i
  M.unsafeWrite (cvBase cv) i (pack (scaleRGB f (unpack old)))

readPixel :: Canvas s -> Int -> Int -> ST s (Maybe RGB)
readPixel cv x y = case index cv x y of
  Nothing -> pure Nothing
  Just i  -> Just . unpack <$> M.unsafeRead (cvBase cv) i

rect :: Canvas s -> Int -> Int -> Int -> Int -> RGB -> ST s ()
rect cv x y w h c = forRect cv x y w h $ \px py -> plot cv px py c

glowRect :: Canvas s -> Int -> Int -> Int -> Int -> RGB -> ST s ()
glowRect cv x y w h c = forRect cv x y w h $ \px py -> glow cv px py c

blendRect :: Canvas s -> Double -> Int -> Int -> Int -> Int -> RGB -> ST s ()
blendRect cv a x y w h c = forRect cv x y w h $ \px py -> blend cv a px py c

darkenRect :: Canvas s -> Double -> Int -> Int -> Int -> Int -> ST s ()
darkenRect cv f x y w h = forRect cv x y w h $ \px py -> darken cv f px py

forRect :: Canvas s -> Int -> Int -> Int -> Int -> (Int -> Int -> ST s ()) -> ST s ()
forRect cv x y w h f = when (inView cv x y w h) $
  forM_ [max y (cvY cv) .. min (y + h - 1) (cvY cv + cvH cv - 1)] $ \py ->
    forM_ [max x (cvX cv) .. min (x + w - 1) (cvX cv + cvW cv - 1)] $ \px -> f px py

mixRGB :: Double -> RGB -> RGB -> RGB
mixRGB a (r1, g1, b1) (r2, g2, b2) = (m r1 r2, m g1 g2, m b1 b2)
  where m p q = round (fromIntegral p * (1 - a) + fromIntegral q * a)

scaleRGB :: Double -> RGB -> RGB
scaleRGB f (r, g, b) = (s r, s g, s b)
  where s c = clamp8 (round (fromIntegral c * f))

luma :: RGB -> Int
luma (r, g, b) = (r * 3 + g * 6 + b) `div` 10

bayer :: U.Vector Double
bayer = U.fromList [ (v + 0.5) / 16 - 0.5 | v <- [0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5] ]

shadePixels :: Frame -> U.Vector Int -> U.Vector Bool -> U.Vector Int
shadePixels fr base lit = U.generate (w * h) px
  where
    w = frameW fr
    h = frameH fr
    lights = [ l | l <- frameLights fr, near l ]
    near l = lightX l + lightRadius l >= fromIntegral (frameX fr)
          && lightX l - lightRadius l <= fromIntegral (frameX fr + w)
          && lightY l + lightRadius l >= fromIntegral (frameY fr)
          && lightY l - lightRadius l <= fromIntegral (frameY fr + h)
    steps = 10 :: Double
    px i
      | U.unsafeIndex lit i = vignette lx ly (U.unsafeIndex base i)
      | otherwise =
          let (r, g, b) = unpack (U.unsafeIndex base i)
              wx = fromIntegral (lx + frameX fr) + 0.5
              wy = fromIntegral (ly + frameY fr) + 0.5
              !(lr, lg, lb) = foldr (add wx wy) (frameAmbient fr, frameAmbient fr, frameAmbient fr * 1.08) lights
              d = U.unsafeIndex bayer ((ly .&. 3) * 4 + (lx .&. 3))
              q v = max 0 (fromIntegral (floor (v * steps + d) :: Int) / steps)
              ch c v = round (fromIntegral c * min 1.7 (q v)) :: Int
          in vignette lx ly (pack (ch r lr, ch g lg, ch b lb))
      where
        lx = i `mod` w
        ly = i `div` w
    add wx wy l (!ar, !ag, !ab) =
      let dx = wx - lightX l
          dy = wy - lightY l
          d2 = dx * dx + dy * dy
          r = lightRadius l
      in if d2 >= r * r then (ar, ag, ab)
         else let f = 1 - sqrt d2 / r
                  k = f * f * lightPower l
                  (cr, cg, cb) = lightColor l
              in (ar + k * cr, ag + k * cg, ab + k * cb)
    vignette lx ly p
      | not (frameVignette fr) = p
      | otherwise =
          let ex = edge lx w
              ey = edge ly h
              f = min ex ey
          in if f >= 1 then p else pack (scaleRGB (0.55 + 0.45 * f) (unpack p))
    edge v n = min 1 (fromIntegral (min v (n - 1 - v)) / 6 :: Double)

canvasImage :: Frame -> (U.Vector Int, U.Vector Bool) -> V.Image
canvasImage fr (base, lit) = V.vertCat [row cy | cy <- [0 .. rows - 1]]
  where
    w = frameW fr
    rows = frameH fr `div` 2
    pixels = shadePixels fr base lit
    labels = IM.fromList
      [ (cy * w + cx, (ch, labelInk l, labelBold l))
      | l <- frameLabels fr
      , let cy = (labelY l - frameY fr) `div` 2
      , (k, ch) <- zip [0 ..] (T.unpack (labelText l))
      , let cx = labelX l - frameX fr + k
      , cx >= 0, cx < w, cy >= 0, cy < rows
      ]
    at x y = U.unsafeIndex pixels (y * w + x)
    row cy = V.horizCat (go 0)
      where
        top x = at x (2 * cy)
        bottom x = at x (2 * cy + 1)
        labelAt x = if IM.null labels then Nothing else IM.lookup (cy * w + x) labels
        go x
          | x >= w = []
          | Just (ch, ink, bold) <- labelAt x =
              let bg = mixRGB 0.5 (unpack (top x)) (unpack (bottom x))
                  a = V.defAttr `V.withForeColor` color ink `V.withBackColor` color bg
              in V.char (if bold then a `V.withStyle` V.bold else a) ch : go (x + 1)
          | otherwise =
              let t = top x
                  b = bottom x
                  n = extend t b (x + 1) - x
                  (ch, a)
                    | t == b    = (' ', V.defAttr `V.withBackColor` color (unpack t))
                    | otherwise = ('▀', V.defAttr `V.withForeColor` color (unpack t) `V.withBackColor` color (unpack b))
              in HorizText a (TL.replicate (fromIntegral n) (TL.singleton ch)) n n : go (x + n)
        extend t b x
          | x < w && top x == t && bottom x == b && isNothingAt x = extend t b (x + 1)
          | otherwise = x
        isNothingAt x = IM.null labels || IM.notMember (cy * w + x) labels
