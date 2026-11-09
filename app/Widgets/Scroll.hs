module Widgets.Scroll
  ( Seg
  , Line (..)
  , Focus (..)
  , Bar (..)
  , Follow (..)
  , Content (..)
  , plainLine
  , segWidth
  , offsets
  , normalize
  , renderContent
  , wrapText
  ) where

import Brick hiding (clamp)
import qualified Data.Text as T

type Seg = (AttrName, T.Text)

data Line = Line
  { lineGutter :: [Seg]
  , lineBody   :: [Seg]
  , lineFill   :: Maybe AttrName
  }

data Focus = Focus
  { focusRow   :: Int
  , focusStart :: Int
  , focusEnd   :: Int
  }

data Follow = Center | Bottom

data Content = Content
  { contentLines  :: [Line]
  , contentGutter :: Int
  , contentFocus  :: Maybe Focus
  , contentFollow :: Follow
  }

data Bar = Bar
  { barOffset :: Int
  , barView   :: Int
  , barTotal  :: Int
  }

plainLine :: [Seg] -> Line
plainLine body = Line [] body Nothing

segWidth :: [Seg] -> Int
segWidth = sum . map (T.length . snd)

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

extent :: Content -> (Int, Int)
extent c = (maximum (0 : map (segWidth . lineBody) (contentLines c)), length (contentLines c))

auto :: Content -> Int -> Int -> (Int, Int)
auto c w h = (ox, oy)
  where
    (cw, ch) = extent c
    bodyW = max 1 (w - contentGutter c)
    oy = case (contentFollow c, contentFocus c) of
      (_, _) | ch <= h -> 0
      (Bottom, Nothing) -> ch - h
      (Bottom, Just f) -> clamp 0 (ch - h) (focusRow f - h + 1)
      (Center, Just f) -> clamp 0 (ch - h) (focusRow f - h `div` 2)
      (Center, Nothing) -> 0
    ox = case contentFocus c of
      Just f | cw > bodyW, focusEnd f > bodyW - 2 -> clamp 0 (cw - bodyW) (focusEnd f - bodyW + 4)
      _ -> 0

offsets :: Content -> Int -> Int -> (Int, Int) -> (Int, Int)
offsets c w h (dx, dy) = (clamp 0 (max 0 (cw - bodyW)) (ax + dx), clamp 0 (max 0 (ch - h)) (ay + dy))
  where
    (cw, ch) = extent c
    bodyW = max 1 (w - contentGutter c)
    (ax, ay) = auto c w h

normalize :: Content -> Int -> Int -> (Int, Int) -> (Int, Int)
normalize c w h delta = (ox - ax, oy - ay)
  where
    (ox, oy) = offsets c w h delta
    (ax, ay) = auto c w h

renderContent :: Content -> Int -> Int -> (Int, Int) -> (Widget n, Maybe Bar, Maybe Bar)
renderContent c w h delta = (body, vbar, hbar)
  where
    (cw, ch) = extent c
    bodyW = max 1 (w - contentGutter c)
    (ox, oy) = offsets c w h delta
    shown = take h (drop oy (contentLines c))
    body = vBox (map row shown ++ [fill ' ' | length shown < h])
    row l = maybe id withDefAttr (lineFill l) $ hBox
      [ hLimit (contentGutter c) (padRight Max (segs (lineGutter l)))
      , padRight Max (segs (crop ox bodyW (lineBody l)))
      ]
    segs [] = str " "
    segs ss = hBox [withAttr a (txt t) | (a, t) <- ss, not (T.null t)]
    vbar = if ch > h then Just (Bar oy h ch) else Nothing
    hbar = if cw > bodyW then Just (Bar ox bodyW cw) else Nothing

crop :: Int -> Int -> [Seg] -> [Seg]
crop skip width = go skip width
  where
    go _ 0 _ = []
    go _ _ [] = []
    go s n ((a, t) : rest)
      | s >= T.length t = go (s - T.length t) n rest
      | otherwise =
          let piece = T.take n (T.drop s t)
          in (a, piece) : go 0 (n - T.length piece) rest

wrapText :: Int -> T.Text -> [T.Text]
wrapText width text = concatMap wrapLine (T.splitOn (T.pack "\n") text)
  where
    w = max 4 width
    wrapLine l
      | T.null l = [T.empty]
      | otherwise = go (T.words l)
    go [] = []
    go ws = let (line, rest) = takeLine ws 0 [] in T.unwords line : go rest
    takeLine [] _ acc = (reverse acc, [])
    takeLine (x : xs) n acc
      | T.length x > w && null acc = ([T.take w x], T.drop w x : xs)
      | n + T.length x + (if null acc then 0 else 1) > w = (reverse acc, x : xs)
      | otherwise = takeLine xs (n + T.length x + (if null acc then 0 else 1)) (x : acc)
