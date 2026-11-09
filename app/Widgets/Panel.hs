module Widgets.Panel
  ( Panel (..)
  , panel
  , fixedPanel
  ) where

import Brick
import Widgets.Scroll (Bar (..))
import Style.Theme

data Panel = Panel
  { panelTitle  :: String
  , panelInfo   :: String
  , panelAccent :: AttrName
  , panelPad    :: Int
  }

panel :: Panel -> (Int -> Int -> (Widget n, Maybe Bar, Maybe Bar)) -> Widget n
panel p body = Widget Greedy Greedy $ do
  ctx <- getContext
  let w = availWidth ctx
      h = availHeight ctx
  render (framed p w h body)

fixedPanel :: Panel -> Int -> Int -> (Int -> Int -> (Widget n, Maybe Bar, Maybe Bar)) -> Widget n
fixedPanel p w h body = framed p w h body

framed :: Panel -> Int -> Int -> (Int -> Int -> (Widget n, Maybe Bar, Maybe Bar)) -> Widget n
framed p w h body
  | w < 6 || h < 3 = emptyWidget
  | otherwise = withDefAttr aPanel $ vBox [top, middle, bottom]
  where
    pad = panelPad p
    iw = w - 2 - 2 * pad
    ih = h - 2
    (content, vbar, hbar) = body iw ih
    title = " " ++ panelTitle p ++ " "
    info = if null (panelInfo p) then "" else " " ++ panelInfo p ++ " "
    room = w - 4 - length title - length info
    top = hBox
      [ edge "╭─"
      , withAttr (panelAccent p) (str "◆")
      , withAttr aPanelTitle (str title)
      , edge (replicate (max 0 (room - 1)) '─')
      , withAttr aPanelInfo (str (take (max 0 (w - 5 - length title)) info))
      , edge "─╮"
      ]
    middle = hBox
      [ vBox (replicate ih (edge "│"))
      , hLimit (iw + 2 * pad) (vLimit ih (padLeftRight pad (hLimit iw (vLimit ih (padRight Max (padBottom Max content))))))
      , vBox [rightEdge i | i <- [0 .. ih - 1]]
      ]
    rightEdge i = case vbar of
      Just b | inThumb b ih i -> withAttr (panelAccent p) (str "┃")
             | otherwise      -> withAttr aPanelTrack (str "│")
      Nothing -> edge "│"
    bottom = hBox
      [ edge "╰─"
      , case hbar of
          Just b  -> hBox [ if inThumb b (w - 4) i then withAttr (panelAccent p) (str "━") else withAttr aPanelTrack (str "─") | i <- [0 .. w - 5] ]
          Nothing -> edge (replicate (w - 4) '─')
      , edge "─╯"
      ]
    edge s = withAttr aPanelEdge (str s)

inThumb :: Bar -> Int -> Int -> Bool
inThumb b track i = i >= start && i < start + size
  where
    total = max 1 (barTotal b)
    size = max 1 (min track ((barView b * track) `div` total))
    maxStart = track - size
    range = max 1 (barTotal b - barView b)
    start = (barOffset b * maxStart + range `div` 2) `div` range
