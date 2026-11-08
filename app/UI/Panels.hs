module UI.Panels (programPanel, statusPanel, helpLine) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.ProgressBar (progressBar)
import qualified Data.Text as T
import Protocol
import UI.Theme

programPanel :: Run -> Int -> Widget Name
programPanel run k = borderWithLabel (str " Program ") $ padLeftRight 1 $ padRight Max $ vBox $
  zipWith line [1 ..] (levelProgram level)
    ++ [str " ", txt (T.pack "?- " <> levelQuery level <> T.pack ".")]
  where
    level = runLevel run
    active = if k > 0 then stepClause (runSteps run !! (k - 1)) else Nothing
    line i text
      | Just i == active = withAttr aLineCurrent (padRight Max (str ("▶ " ++ show i ++ "  ") <+> txt text))
      | otherwise        = withAttr aLine (str ("  " ++ show i ++ "  ") <+> txt text)

statusPanel :: Run -> Int -> Widget Name
statusPanel run k = borderWithLabel (str " Status ") $ padLeftRight 1 $ vBox
  [ str "Torch"
  , progressBar (Just (show left ++ " / " ++ show torch)) (fromIntegral left / fromIntegral torch)
  , str " "
  , withAttr aMessage (txtWrap message)
  ]
  where
    total = length (runSteps run)
    torch = levelTorch (runLevel run)
    left  = torch - torchUsed run k
    message
      | k == 0     = levelDescription (runLevel run)
      | k == total = stepText (runSteps run !! (k - 1)) <> T.pack "\n\n" <> runMessage run
      | otherwise  = stepText (runSteps run !! (k - 1))

torchUsed :: Run -> Int -> Int
torchUsed run k = length [s | s <- take k (runSteps run), stepEvent s `notElem` free]
  where free = [T.pack "success", T.pack "trap"]

helpLine :: Widget Name
helpLine = padLeftRight 1 $ hBox
  [ key "←/→", str " step   ", key "Home/End", str " start/end   "
  , key "1-9", str " level   ", key "q", str " quit" ]
  where key k = withAttr aKey (str (" " ++ k ++ " "))
