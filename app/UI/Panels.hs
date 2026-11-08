module UI.Panels (programPanel, statusPanel, starsText, parText, eventIcon) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.ProgressBar (progressBar, progressCompleteAttr)
import qualified Data.Text as T
import Editor
import Protocol
import UI.Pixel (spriteImage)
import UI.Sprites (flame1, flame2, torchOut)
import UI.Syntax
import UI.Theme

programPanel :: Mode -> Run -> Int -> Widget Name
programPanel mode run k = borderWithLabel (withAttr aTitle (str label)) $ padLeftRight 1 $ padRight Max $ vBox $
  case mode of
    Replay              -> replayLines run k
    Editing cursor pick -> editLines (runLevel run) cursor pick
  where
    label = case mode of
      Replay -> " Program "
      _      -> " Program · editing "

replayLines :: Run -> Int -> [Widget Name]
replayLines run k =
  zipWith line [1 ..] (levelProgram level)
    ++ [str " ", withAttr aGold (str "?- ") <+> highlight (levelQuery level) <+> withAttr aPunct (str ".")]
  where
    level  = runLevel run
    active = if k > 0 then stepClause (runSteps run !! (k - 1)) else Nothing
    line i text
      | Just i == active = withDefAttr aLineCurrent (padRight Max (withAttr aGold (str ("▶ " ++ show i ++ "  ")) <+> highlight text))
      | otherwise        = withAttr aDim (str ("  " ++ show i ++ "  ")) <+> highlight text

editLines :: Level -> Cursor -> Maybe Pick -> [Widget Name]
editLines level cursor pick =
  zipWith clauseLine [1 ..] (levelClauses level)
    ++ [str " ", queryRow (length (levelClauses level) + 1)]
  where
    token row col text
      | cursor == Cursor row col = withAttr aCursor (txt text)
      | picked row col           = withAttr aPicked (txt text)
      | otherwise                = highlight text
    picked row col = case pick of
      Just (PickClause r) -> r == row
      Just (PickGoal r c) -> r == row && c == col
      Nothing             -> False
    endSlot row goals
      | cursor == Cursor row (length goals + 1) = withAttr aCursor (str " ")
      | otherwise                               = emptyWidget
    goalTokens row goals = hBox (punctuate (withAttr aPunct (str ", ")) [token row c g | (c, g) <- zip [1 ..] goals])
    clauseLine row parts = hBox
      [ withAttr aDim (str ("  " ++ show row ++ "  "))
      , token row 0 (clauseHead parts)
      , if null (clauseGoals parts) then emptyWidget else withAttr aPunct (str " :- ")
      , goalTokens row (clauseGoals parts)
      , endSlot row (clauseGoals parts)
      , withAttr aPunct (str ".")
      ]
    queryRow row = hBox
      [ withAttr aGold (str "?- "), goalTokens row (levelQueryGoals level)
      , endSlot row (levelQueryGoals level), withAttr aPunct (str ".") ]

punctuate :: Widget n -> [Widget n] -> [Widget n]
punctuate _ []       = []
punctuate _ [w]      = [w]
punctuate s (w : ws) = w : s : punctuate s ws

statusPanel :: Int -> Bool -> Run -> Int -> Widget Name
statusPanel frame hint run k = borderWithLabel (withAttr aTitle (str " Status ")) $ padLeftRight 1 $ vBox
  [ hBox [ raw (spriteImage icon), padLeft (Pad 1) (vBox [str " ", bar, str " "]) ]
  , str " "
  , hBox [withAttr aLabel (str "Tools  "), hBox (map toolWidget (levelTools level))]
  , hBox [withAttr aLabel (str "Edits  "), withAttr aValue (str (show (levelEdits level)))]
  , hBox [withAttr aLabel (str "Par    "), withAttr aValue (str (parText (levelPar level)))]
  , str " "
  , if hint
      then withAttr aHint (txtWrap (hintText (levelHint level)))
      else message
  ]
  where
    level = runLevel run
    total = length (runSteps run)
    torch = levelTorch level
    left  = torch - torchUsed run k
    ratio = fromIntegral left / fromIntegral torch :: Float
    icon
      | left <= 0  = torchOut
      | even frame = flame1
      | otherwise  = flame2
    barAttr
      | ratio > 0.5  = aBarHigh
      | ratio > 0.25 = aBarMid
      | otherwise    = aBarLow
    bar = overrideAttr progressCompleteAttr barAttr
            (progressBar (Just (show left ++ " / " ++ show torch)) ratio)
    message
      | k == 0    = withAttr aMessage (txtWrap (levelDescription level))
      | otherwise =
          let s = runSteps run !! (k - 1)
              (glyph, attr) = eventIcon (stepEvent s)
              done = if k == total then T.pack "\n\n" <> runMessage run else T.empty
          in withAttr attr (str (glyph ++ " ")) <+> withAttr aMessage (txtWrap (stepText s <> done))

eventIcon :: T.Text -> (String, AttrName)
eventIcon event = case T.unpack event of
  "enter"        -> ("▾", aGold)
  "mismatch"     -> ("×", aDim)
  "backtrack"    -> ("↶", aSteel)
  "cut"          -> ("▣", aCrimson)
  "not_start"    -> ("✦", aArcane)
  "not_fails"    -> ("✦", aArcane)
  "not_succeeds" -> ("✦", aArcane)
  "success"      -> ("◆", aGold)
  "trap"         -> ("☠", aCrimson)
  _              -> ("·", aDim)

toolWidget :: Tool -> Widget Name
toolWidget t = hBox
  [ withAttr attr (str (glyph ++ " "))
  , withAttr (if toolUses t > 0 then aValue else aDim) (str (name ++ " ×" ++ show (toolUses t) ++ "   "))
  ]
  where
    (glyph, attr, name) = case T.unpack (toolName t) of
      "move_clauses" -> ("⇅", aGold, "move clause")
      "move_goals"   -> ("⇄", aGold, "move goal")
      "add_cut"      -> ("▣", aCrimson, "add cut")
      "remove_cut"   -> ("▢", aCrimson, "remove cut")
      other          -> ("·", aDim, other)

hintText :: T.Text -> T.Text
hintText h
  | T.null h  = T.pack "No hint for this level."
  | otherwise = T.pack "Hint: " <> h

parText :: Maybe Par -> String
parText Nothing    = "none"
parText (Just par) = show (parTorch par) ++ " torch, " ++ show (parEdits par) ++ " edits"

starsText :: Int -> String
starsText n = replicate n '★' ++ replicate (3 - n) '☆'

torchUsed :: Run -> Int -> Int
torchUsed run k = length [s | s <- take k (runSteps run), stepEvent s `notElem` free]
  where free = [T.pack "success", T.pack "trap"]
