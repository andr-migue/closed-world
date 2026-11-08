module UI.Panels
  ( programContent
  , queryContent
  , chronicleContent
  , programPanel
  , queryPanel
  , chroniclePanel
  , statusPanel
  , hintPanel
  , parText
  , eventIcon
  ) where

import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Editor
import Protocol
import State
import UI.Frame
import UI.Palette
import UI.Pixel (spriteImage)
import UI.Scroll
import UI.Sprites (flame1, flame2, torchOut)
import UI.Syntax (segments)
import UI.Theme

activeClause :: Run -> Int -> Maybe Int
activeClause run k
  | k > 0 && k <= length (runSteps run) = stepClause (runSteps run !! (k - 1))
  | otherwise = Nothing

programContent :: Mode -> Run -> Int -> Content
programContent m run k = case m of
  Replay -> Content replayLines gutterW (fmap (\i -> Focus (i - 1) 0 0) active) Center
  Editing cursor pick ->
    let rows = [editRow level cursor pick r (clauseTokens parts) | (r, parts) <- zip [1 ..] (levelClauses level)]
        focus = case [Focus (r - 1) s e | (r, (_, Just (s, e))) <- zip [1 ..] rows] of
          (f : _) -> Just f
          []      -> Nothing
        gut r = if curRow cursor == r then [(aGutterCurrent, T.pack (pad r ++ " "))] else [(aGutter, T.pack (pad r ++ " "))]
    in Content [ Line (gut r) body (if curRow cursor == r then Just aRowCursor else Nothing) | (r, (body, _)) <- zip [1 ..] rows ]
               gutterW focus Center
  where
    level = runLevel run
    n = length (levelProgram level)
    digits = length (show (max 1 n))
    gutterW = digits + 4
    pad i = replicate (digits - length (show i)) ' ' ++ show (i :: Int)
    active = activeClause run k
    replayLines =
      [ if Just i == active
          then Line [(aGutterCurrent, T.pack ("▸ " ++ pad i ++ " "))] (segments text) (Just aLineCurrent)
          else Line [(aGutter, T.pack ("  " ++ pad i ++ " "))] (segments text) Nothing
      | (i, text) <- zip [1 ..] (levelProgram level) ]

clauseTokens :: ClauseParts -> (T.Text, [T.Text])
clauseTokens parts = (clauseHead parts, clauseGoals parts)

editRow :: Level -> Cursor -> Maybe Pick -> Int -> (T.Text, [T.Text]) -> ([Seg], Maybe (Int, Int))
editRow _ cursor pick row (headT, goals) = (concat pieces, focus)
  where
    tokens = (0, headT, True) : [(c, g, False) | (c, g) <- zip [1 ..] goals]
    sepAfter (c, _, isHead)
      | isHead = if null goals then [] else [(aPunct, T.pack " :- ")]
      | c < length goals = [(aPunct, T.pack ", ")]
      | otherwise = []
    endCol = length goals + 1
    endSlot = if cursor == Cursor row endCol then [(aCursor, T.pack " ")] else []
    styled (c, t, _)
      | cursor == Cursor row c = [(aCursor, t)]
      | picked c = [(aPicked, t)]
      | otherwise = segments t
    picked c = case pick of
      Just (PickClause r) -> r == row
      Just (PickGoal r c') -> r == row && c' == c
      Nothing -> False
    pieces = [styled tok ++ sepAfter tok | tok <- tokens] ++ [endSlot, [(aPunct, T.pack ".")]]
    widths = scanl (+) 0 (map segWidth pieces)
    focus
      | curRow cursor /= row = Nothing
      | otherwise =
          let i = min (length tokens) (curCol cursor)
              s = widths !! i
          in Just (s, s + max 1 (segWidth (concat (take 1 (drop i pieces)))))

queryContent :: Mode -> Run -> Content
queryContent m run = case m of
  Editing cursor pick | isQueryRow level (curRow cursor) ->
    let row = curRow cursor
        goals = levelQueryGoals level
        toks = [(c, g) | (c, g) <- zip [1 ..] goals]
        styled (c, t)
          | cursor == Cursor row c = [(aCursor, t)]
          | picked c = [(aPicked, t)]
          | otherwise = segments t
        picked c = case pick of
          Just (PickGoal r c') -> r == row && c' == c
          _ -> False
        pieces = [(aGold, T.pack "?- ") : []]
                 ++ [styled tok ++ [(aPunct, T.pack ", ") | fst tok < length goals] | tok <- toks]
                 ++ [[(aCursor, T.pack " ") | curCol cursor == length goals + 1], [(aPunct, T.pack ".")]]
        widths = scanl (+) 0 (map segWidth pieces)
        i = min (length goals + 1) (curCol cursor)
        s = widths !! i
    in Content [Line [] (concat pieces) (Just aRowCursor)] 0 (Just (Focus 0 s (s + max 1 (segWidth (pieces !! i))))) Center
  _ -> Content [plainLine ((aGold, T.pack "?- ") : segments (levelQuery level) ++ [(aPunct, T.pack ".")])] 0 Nothing Center
  where level = runLevel run

chronicleContent :: Run -> Int -> Int -> Content
chronicleContent run k width = Content (intro ++ concat entries ++ outro) 5 focus Bottom
  where
    level = runLevel run
    total = length (runSteps run)
    bodyW = max 8 (width - 5)
    intro = [ Line [] [(aMessage, l)] Nothing | l <- wrapText (bodyW - 1) (levelDescription level) ] ++ [Line [] [] Nothing]
    entries = [ entry i s | (i, s) <- zip [1 ..] (take k (runSteps run)) ]
    entry i s =
      let (glyph, attr) = eventIcon (stepEvent s)
          ls = wrapText (bodyW - 2) (stepText s)
          fillA = if i == k then Just aEntryCurrent else Nothing
          numA = if i == k then aGutterCurrent else aGutter
          first = Line [(numA, T.pack (padL 3 (show i) ++ " "))] ((attr, T.pack (glyph ++ " ")) : [(if i == k then aLine else aKeyLabel, head (ls ++ [T.empty]))]) fillA
          rest = [ Line [] [(aDim, T.pack "  "), (if i == k then aLine else aKeyLabel, l)] fillA | l <- drop 1 ls ]
      in first : rest
    outro
      | k == total && total > 0 =
          Line [] [] Nothing : [ Line [] [(resultAttr, l)] Nothing | l <- wrapText (bodyW - 1) (runMessage run) ]
      | otherwise = []
    resultAttr = if runResult run == T.pack "success" then aGold else aCrimson
    allLines = intro ++ concat entries ++ outro
    focus = Just (Focus (length allLines - 1) 0 0)

padL :: Int -> String -> String
padL n s = replicate (n - length s) ' ' ++ s

programPanel :: Mode -> Run -> Int -> (Int, Int) -> Widget Name
programPanel m run k delta = clickable ProgramView $ reportExtent ProgramView $
  panel (Panel "PROGRAM" info aGold 1) $ \w h -> renderContent (programContent m run k) w h delta
  where
    info = case m of
      Editing _ (Just _) -> "drop it somewhere"
      Editing _ Nothing  -> "editing"
      Replay -> maybe (show (length (levelProgram (runLevel run))) ++ " clauses") (\c -> "clause " ++ show c) (activeClause run k)

queryPanel :: Mode -> Run -> (Int, Int) -> Widget Name
queryPanel m run delta = clickable QueryView $ reportExtent QueryView $ vLimit 3 $
  panel (Panel "QUERY" "" aGold 1) $ \w h -> renderContent (queryContent m run) w h delta

chroniclePanel :: Run -> Int -> (Int, Int) -> Widget Name
chroniclePanel run k delta = clickable ChronicleView $ reportExtent ChronicleView $
  panel (Panel "CHRONICLE" (if k == 0 then "" else "step " ++ show k ++ "/" ++ show (length (runSteps run))) aArcane 1) $
    \w h -> renderContent (chronicleContent run k w) w h delta

hintPanel :: Run -> Widget Name
hintPanel run = Widget Greedy Fixed $ do
  ctx <- getContext
  let w = availWidth ctx
      ls = wrapText (w - 5) (hintText (levelHint (runLevel run)))
  render $ fixedPanel (Panel "HINT" "h to hide" aEmerald 1) w (length ls + 2) $ \_ _ ->
    (vBox [withAttr aHint (txt l) | l <- ls], Nothing, Nothing)

hintText :: T.Text -> T.Text
hintText h
  | T.null h  = T.pack "No hint for this level."
  | otherwise = h

statusPanel :: Game -> Run -> Int -> Widget Name
statusPanel game run k = Widget Greedy Fixed $ do
  ctx <- getContext
  let w = availWidth ctx
      iw0 = w - 4
      chipRows = packChips (iw0 - 6) (map toolChip (levelTools level))
  render $ fixedPanel (Panel "TORCH" (if left <= 0 then "burnt out" else "") aFlame 1) w (8 + length chipRows) $ \iw _ ->
    ( vBox $
        [ hBox [ raw (spriteImage icon), str "  ", vBox [ raw (torchBar (iw - 9) ratio (frame game)), counts iw ] ]
        , str " "
        ]
        ++ [ hBox (withAttr aLabel (str (if i == 0 then "Tools " else "      ")) : map snd row) | (i, row) <- zip [0 :: Int ..] chipRows ]
        ++ [ hBox [ withAttr aLabel (str "Edits "), withAttr aValue (str (show (levelEdits level)))
                  , withAttr aDim (str "   par "), withAttr aValue (str (parText (levelPar level))) ] ]
    , Nothing, Nothing )
  where
    level = runLevel run
    torch = levelTorch level
    left  = torchLeft run k
    ratio = fromIntegral (max 0 left) / fromIntegral (max 1 torch) :: Double
    icon
      | left <= 0 = torchOut
      | even (frame game `div` 3) = flame1
      | otherwise = flame2
    counts iw = hBox
      [ withAttr aValue (str (show (max 0 left))), withAttr aDim (str (" / " ++ show torch))
      , padLeft Max (hBox [withAttr aDim (str "speed "), withAttr aGold (str (replicate (speed game + 1) '▸')), withAttr aPanelTrack (str (replicate (3 - speed game) '▸'))])
      ] & hLimit (iw - 9)

(&) :: a -> (a -> b) -> b
x & f = f x

packChips :: Int -> [(Int, Widget Name)] -> [[(Int, Widget Name)]]
packChips width = go 0 []
  where
    go _ acc [] = [reverse acc | not (null acc)]
    go used acc (c@(n, _) : rest)
      | null acc || used + n <= width = go (used + n) (c : acc) rest
      | otherwise = reverse acc : go 0 [] (c : rest)

toolChip :: Tool -> (Int, Widget Name)
toolChip t = (length label + 1, hBox [withAttr attr (str label), str " "])
  where
    label = " " ++ glyph ++ " " ++ name ++ " ×" ++ show (toolUses t) ++ " "
    used = toolUses t <= 0
    (glyph, base, name) = case T.unpack (toolName t) of
      "move_clauses" -> ("⇅", aChip, "clause")
      "move_goals"   -> ("⇄", aChip, "goal")
      "add_cut"      -> ("!", aChipCut, "cut")
      "remove_cut"   -> ("✂", aChipCut, "uncut")
      other          -> ("·", aChip, other)
    attr = if used then aChipOff else base

torchBar :: Int -> Double -> Int -> V.Image
torchBar w ratio t = V.vertCat [row 0, row 1]
  where
    width = max 6 w
    filled = round (ratio * fromIntegral (width - 2)) :: Int
    (hi, base, lo) =
      if ratio > 0.5 then ((255, 222, 130), (255, 160, 60), (212, 100, 40))
      else if ratio > 0.25 then ((255, 170, 90), (222, 92, 52), (150, 52, 36))
      else ((255, 120, 130), (214, 58, 82), (132, 30, 56))
    edgeC = (70, 60, 96)
    empty = (34, 30, 48)
    shimmer = (t `div` 2) `mod` (width + 10)
    px x y
      | x == 0 || x == width - 1 = if y == 0 || y == 3 then Nothing else Just edgeC
      | y == 0 || y == 3 = Just edgeC
      | x - 1 < filled =
          let c = if y == 1 then hi else if y == 2 then base else lo
          in Just (if abs (x - shimmer) <= 1 && y == 1 then (255, 250, 230) else c)
      | otherwise = Just (if y == 1 then (42, 38, 58) else empty)
    row r = V.horizCat [cell (px x (2 * r)) (px x (2 * r + 1)) | x <- [0 .. width - 1]]
    cell (Just a) (Just b) = V.char (V.defAttr `V.withForeColor` color a `V.withBackColor` color b) '▀'
    cell (Just a) Nothing  = V.char (V.defAttr `V.withForeColor` color a `V.withBackColor` color panelBg) '▀'
    cell Nothing (Just b)  = V.char (V.defAttr `V.withForeColor` color b `V.withBackColor` color panelBg) '▄'
    cell Nothing Nothing   = V.char (V.defAttr `V.withBackColor` color panelBg) ' '

parText :: Maybe Par -> String
parText Nothing    = "none"
parText (Just par) = show (parTorch par) ++ " torch · " ++ show (parEdits par) ++ " edits"

eventIcon :: T.Text -> (String, AttrName)
eventIcon event = case T.unpack event of
  "enter"        -> ("▾", aGold)
  "mismatch"     -> ("×", aDim)
  "backtrack"    -> ("↶", aSteel)
  "cut"          -> ("!", aCrimson)
  "not_start"    -> ("✦", aArcane)
  "not_fails"    -> ("✦", aArcane)
  "not_succeeds" -> ("✦", aArcane)
  "success"      -> ("◆", aGold)
  "trap"         -> ("☠", aCrimson)
  _              -> ("·", aDim)
