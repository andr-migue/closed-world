module UI.Play (playScreen, resultDialog, keyBar, chip, header) where

import Brick
import Brick.Widgets.Center (centerLayer, hCenter)
import qualified Data.Text as T
import Editor
import Protocol
import State
import UI.Dungeon.Scene
import UI.Frame
import UI.Palette
import UI.Panels
import UI.Pixel (spriteImage)
import UI.PixelFont (banner)
import UI.Sprites (chest, rubble, skull, starsImage, torchOut)
import UI.Theme

playScreen :: Game -> Run -> Widget Name
playScreen game run = vBox
  [ header game (Just run)
  , hBox
      [ mapPanel game run
      , hLimit 50 $ vBox $
          [ statusPanel game run k
          , programPanel (mode game) run k (scrollOf ProgramView game)
          , queryPanel (mode game) run (scrollOf QueryView game)
          ]
          ++ [ hintPanel run | hint game ]
          ++ [ vLimitPercent 38 (chroniclePanel run k (scrollOf ChronicleView game)) ]
      ]
  , footer game
  ]
  where k = stepIx game

mapPanel :: Game -> Run -> Widget Name
mapPanel game run = clickable DungeonView $ panel (Panel "DUNGEON" info aGold 0) $ \w h ->
  (hLimit w (vLimit h (dungeon scene)), Nothing, Nothing)
  where
    k = stepIx game
    level = runLevel run
    scene = Scene
      { scRun = run
      , scStep = k
      , scProgress = if k == 0 then 1 else progress game
      , scTime = frame game
      , scPan = pan game
      , scTorch = fromIntegral (max 0 (torchLeft run k)) / fromIntegral (max 1 (levelTorch level))
      , scMinimap = minimap game
      }
    info
      | pan game /= (0, 0) = "panned · step to follow"
      | otherwise = T.unpack (levelConcept level)

header :: Game -> Maybe Run -> Widget Name
header game mrun = vLimit 1 $ withDefAttr aHeader $ hBox $
  [ withAttr aBrand (str " ◆ CLOSED WORLD ") ]
  ++ case mrun of
       Just run ->
         [ withAttr aDim (str ("  Level " ++ show (levelId game) ++ "/" ++ show (length (levels game)) ++ "  "))
         , withAttr aPanelTitle (txt (levelName (runLevel run)))
         , withAttr aHeader (fill ' ')
         , hBox (status run)
         ]
       Nothing -> [withAttr aHeader (fill ' ')]
  where
    status run =
      [ withAttr aDim (str "step ")
      , withAttr aValue (str (show (stepIx game)))
      , withAttr aDim (str ("/" ++ show (length (runSteps run)) ++ "   "))
      , withAttr (if playing game then aGold else aDim) (str (if playing game then "▶ playing " else "❚❚ paused "))
      , withAttr aHeader (str " ")
      ]

footer :: Game -> Widget Name
footer game
  | not (null (notice game)) = vLimit 1 (hBox [withAttr aNotice (str ("  ⚠ " ++ notice game ++ "  ")), withAttr aHeader (fill ' ')])
  | otherwise = keyBar $ case mode game of
      Editing _ _ ->
        [ ("arrows", "select"), ("space", "pick/drop"), ("c", "cut"), ("x", "uncut")
        , ("u", "undo"), ("R", "reset"), ("pgup/dn", "scroll"), ("enter", "run"), ("esc", "done") ]
      Replay
        | stepIx game == 0 && not (playing game) ->
            [ ("space", "release the adventurer"), ("e", "edit"), ("h", "hint"), ("m", "map")
            , ("?", "help"), ("esc", "levels") ]
        | otherwise ->
            [ ("space", if playing game then "pause" else "play"), ("←/→", "step"), ("+/-", "speed")
            , ("wasd", "pan"), ("e", "edit"), ("h", "hint"), ("pgup/dn", "scroll"), ("?", "help"), ("esc", "levels") ]

keyBar :: [(String, String)] -> Widget Name
keyBar pairs = vLimit 1 $ hBox (withAttr aHeader (str " ") : map (uncurry chip) pairs ++ [withAttr aHeader (fill ' ')])

chip :: String -> String -> Widget Name
chip k label = withAttr aKey (str (" " ++ k ++ " ")) <+> withAttr aChipLabel (str (" " ++ label ++ "  "))

resultDialog :: Game -> Run -> Widget Name
resultDialog game run = centerLayer $ fixedPanel (Panel title "" accent 2) 76 (if won then 20 else 16) $ \_ _ ->
  ( vBox $
      [ str " "
      , hCenter (hBox [raw (spriteImage icon), str "   ", raw (banner ink outline word)])
      , str " "
      , hCenter (withAttr aMessage (txt (subtitle (runMessage run))))
      ]
      ++ [ padTop (Pad 1) (hCenter (raw (starsImage (min (runStars run) ((frame game - endFrame game) `div` 6))))) | won ]
      ++ [ str " "
         , hCenter (hBox [ withAttr aLabel (str "Torch used "), withAttr aValue (str (show (runTorchUsed run)))
                         , withAttr aDim (str (" / " ++ show (levelTorch level) ++ "     "))
                         , withAttr aLabel (str "Edits "), withAttr aValue (str (show (levelEdits level))) ])
         , hCenter (withAttr aDim (str ("Par: " ++ parText (levelPar level))))
         , str " "
         , hCenter (hBox (actions game))
         ]
  , Nothing, Nothing )
  where
    level = runLevel run
    won   = succeeded game
    title = if won then "TREASURE" else "THE ADVENTURER FAILED"
    accent = if won then aGold else aCrimson
    (word, icon, ink) = case T.unpack (runResult run) of
      "success"   -> ("TREASURE", chest, inkOf goldLight gold goldDeep)
      "trap"      -> ("TRAP", skull, inkOf (255, 120, 130) crimson crimsonDeep)
      "torch_out" -> ("TORCH OUT", torchOut, inkOf flameLight ember crimsonDeep)
      _           -> ("NO WAY OUT", rubble, inkOf steel wallLight wallShadow)

subtitle :: T.Text -> T.Text
subtitle m = maybe m T.strip (T.stripPrefix (T.pack "TREASURE!") m)

inkOf :: RGB -> RGB -> RGB -> Int -> RGB
inkOf top middle bottom y
  | y <= 1    = top
  | y <= 4    = middle
  | otherwise = bottom

actions :: Game -> [Widget Name]
actions game
  | succeeded game =
      [ chip "n" "next level" | hasNextLevel game ]
      ++ [ chip "e" "improve", chip "space" "watch again", chip "esc" "levels" ]
  | otherwise =
      [ chip "e" "edit", chip "h" "hint", chip "space" "watch again", chip "esc" "levels" ]
