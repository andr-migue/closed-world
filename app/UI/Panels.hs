module UI.Panels (programPanel, statusPanel, starsText, parText) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.ProgressBar (progressBar)
import qualified Data.Text as T
import Editor
import Protocol
import UI.Theme

programPanel :: Mode -> Run -> Int -> Widget Name
programPanel mode run k = borderWithLabel (str label) $ padLeftRight 1 $ padRight Max $ vBox $
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
    ++ [str " ", txt (T.pack "?- " <> levelQuery level <> T.pack ".")]
  where
    level  = runLevel run
    active = if k > 0 then stepClause (runSteps run !! (k - 1)) else Nothing
    line i text
      | Just i == active = withAttr aLineCurrent (padRight Max (str ("▶ " ++ show i ++ "  ") <+> txt text))
      | otherwise        = withAttr aLine (str ("  " ++ show i ++ "  ") <+> txt text)

editLines :: Level -> Cursor -> Maybe Pick -> [Widget Name]
editLines level cursor pick =
  zipWith clauseLine [1 ..] (levelClauses level)
    ++ [str " ", queryLine (length (levelClauses level) + 1)]
  where
    token row col text = withAttr (tokenAttr row col) (txt text)
    tokenAttr row col
      | cursor == Cursor row col = aCursor
      | picked row col           = aPicked
      | otherwise                = aLine
    picked row col = case pick of
      Just (PickClause r) -> r == row
      Just (PickGoal r c) -> r == row && c == col
      Nothing             -> False
    endSlot row goals
      | cursor == Cursor row (length goals + 1) = withAttr aCursor (str " ")
      | otherwise                               = emptyWidget
    goalTokens row goals = hBox (punctuate (str ", ") [token row c g | (c, g) <- zip [1 ..] goals])
    clauseLine row parts = withAttr aLine $ hBox
      [ str ("  " ++ show row ++ "  ")
      , token row 0 (clauseHead parts)
      , if null (clauseGoals parts) then emptyWidget else str " :- "
      , goalTokens row (clauseGoals parts)
      , endSlot row (clauseGoals parts)
      , str "."
      ]
    queryLine row = withAttr aLine $ hBox
      [ str "?- ", goalTokens row (levelQueryGoals level), endSlot row (levelQueryGoals level), str "." ]

punctuate :: Widget n -> [Widget n] -> [Widget n]
punctuate _ []       = []
punctuate _ [w]      = [w]
punctuate s (w : ws) = w : s : punctuate s ws

statusPanel :: Bool -> Run -> Int -> Widget Name
statusPanel hint run k = borderWithLabel (str " Status ") $ padLeftRight 1 $ vBox
  [ str "Torch"
  , progressBar (Just (show left ++ " / " ++ show torch)) (fromIntegral left / fromIntegral torch)
  , str " "
  , str ("Tools  " ++ toolsText (levelTools level))
  , str ("Edits  " ++ show (levelEdits level))
  , str ("Par    " ++ parText (levelPar level))
  , str " "
  , if hint
      then withAttr aHint (txtWrap (hintText (levelHint level)))
      else withAttr aMessage (txtWrap message)
  ]
  where
    level = runLevel run
    total = length (runSteps run)
    torch = levelTorch level
    left  = torch - torchUsed run k
    message
      | k == 0     = levelDescription level
      | k == total = stepText (runSteps run !! (k - 1)) <> T.pack "\n\n" <> runMessage run
      | otherwise  = stepText (runSteps run !! (k - 1))

hintText :: T.Text -> T.Text
hintText h
  | T.null h  = T.pack "No hint for this level."
  | otherwise = T.pack "Hint: " <> h

parText :: Maybe Par -> String
parText Nothing    = "none"
parText (Just par) = show (parTorch par) ++ " torch, " ++ show (parEdits par) ++ " edits"

starsText :: Int -> String
starsText n = replicate n '★' ++ replicate (3 - n) '☆'

toolsText :: [Tool] -> String
toolsText [] = "none"
toolsText tools = unwords [T.unpack (toolName t) ++ " ×" ++ show (toolUses t) | t <- tools]

torchUsed :: Run -> Int -> Int
torchUsed run k = length [s | s <- take k (runSteps run), stepEvent s `notElem` free]
  where free = [T.pack "success", T.pack "trap"]
