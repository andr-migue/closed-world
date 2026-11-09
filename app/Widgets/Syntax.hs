module Widgets.Syntax (segments) where

import Brick (AttrName)
import Data.Char (isAlphaNum, isUpper)
import qualified Data.Text as T
import Style.Theme

data Kind = Functor | Atom | Variable | Keyword | Punct

segments :: T.Text -> [(AttrName, T.Text)]
segments = map (\(k, t) -> (attrOf k, t)) . classify . tokens

tokens :: T.Text -> [T.Text]
tokens t
  | T.null t  = []
  | isWord (T.head t) = let (w, rest) = T.span isWord t in w : tokens rest
  | otherwise = let (c, rest) = T.splitAt 1 t in c : tokens rest
  where isWord c = isAlphaNum c || c == '_'

classify :: [T.Text] -> [(Kind, T.Text)]
classify (w : rest@(next : _))
  | isName w && next == T.pack "(" = (kindOf w Functor, w) : classify rest
classify (w : rest) = (kindOf w Atom, w) : classify rest
classify [] = []

kindOf :: T.Text -> Kind -> Kind
kindOf w named
  | w == T.pack "!"                       = Keyword
  | w `elem` map T.pack ["not", "trap"]   = Keyword
  | not (isName w)                        = Punct
  | isUpper (T.head w) || T.head w == '_' = Variable
  | otherwise                             = named

isName :: T.Text -> Bool
isName w = not (T.null w) && (isAlphaNum (T.head w) || T.head w == '_')

attrOf :: Kind -> AttrName
attrOf Functor  = aFunctor
attrOf Atom     = aAtom
attrOf Variable = aVariable
attrOf Keyword  = aKeyword
attrOf Punct    = aPunct
