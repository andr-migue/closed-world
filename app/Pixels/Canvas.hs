{-# LANGUAGE RankNTypes #-}
module Pixels.Canvas
  ( Canvas
  , Viewport (..)
  , Picture (..)
  , paint
  , inView
  , plot
  , glow
  , blend
  , darken
  , darkenRect
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Pixels.Color
import Style.Colors (RGB)

data Viewport = Viewport
  { portW :: Int
  , portH :: Int
  , portX :: Int
  , portY :: Int
  }

data Picture = Picture
  { picColors  :: U.Vector Pixel
  , picGlowing :: U.Vector Bool
  }

data Canvas s = Canvas
  { cvPort    :: Viewport
  , cvColors  :: M.MVector s Pixel
  , cvGlowing :: M.MVector s Bool
  }

paint :: Viewport -> RGB -> (forall s. Canvas s -> ST s ()) -> Picture
paint port background draw = runST $ do
  colors  <- M.replicate (portW port * portH port) (pack background)
  glowing <- M.replicate (portW port * portH port) False
  draw (Canvas port colors glowing)
  Picture <$> U.freeze colors <*> U.freeze glowing

index :: Canvas s -> Int -> Int -> Maybe Int
index cv x y
  | lx < 0 || ly < 0 || lx >= portW port || ly >= portH port = Nothing
  | otherwise = Just (ly * portW port + lx)
  where
    port = cvPort cv
    lx = x - portX port
    ly = y - portY port

inView :: Canvas s -> Int -> Int -> Int -> Int -> Bool
inView cv x y w h =
  x + w > portX port && y + h > portY port && x < portX port + portW port && y < portY port + portH port
  where port = cvPort cv

plot :: Canvas s -> Int -> Int -> RGB -> ST s ()
plot cv x y c = forM_ (index cv x y) $ \i -> do
  M.unsafeWrite (cvColors cv) i (pack c)
  M.unsafeWrite (cvGlowing cv) i False

glow :: Canvas s -> Int -> Int -> RGB -> ST s ()
glow cv x y c = forM_ (index cv x y) $ \i -> do
  M.unsafeWrite (cvColors cv) i (pack c)
  M.unsafeWrite (cvGlowing cv) i True

blend :: Canvas s -> Double -> Int -> Int -> RGB -> ST s ()
blend cv a x y c = forM_ (index cv x y) $ \i -> do
  old <- M.unsafeRead (cvColors cv) i
  M.unsafeWrite (cvColors cv) i (pack (mixRGB a (unpack old) c))

darken :: Canvas s -> Double -> Int -> Int -> ST s ()
darken cv f x y = forM_ (index cv x y) $ \i -> do
  old <- M.unsafeRead (cvColors cv) i
  M.unsafeWrite (cvColors cv) i (pack (scaleRGB f (unpack old)))

darkenRect :: Canvas s -> Double -> Int -> Int -> Int -> Int -> ST s ()
darkenRect cv f x y w h = when (inView cv x y w h) $
  forM_ [max y (portY port) .. min (y + h - 1) (portY port + portH port - 1)] $ \py ->
    forM_ [max x (portX port) .. min (x + w - 1) (portX port + portW port - 1)] $ \px -> darken cv f px py
  where port = cvPort cv
