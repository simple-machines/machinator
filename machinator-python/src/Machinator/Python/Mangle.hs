{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Mangle where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Machinator.Core.Data.Definition

import           P

-- -----------------------------------------------------------------------------

-- | Python keywords
--
-- https://docs.python.org/3/reference/lexical_analysis.html#keywords
pythonKeywords :: Set T.Text
pythonKeywords =
  S.fromList [
    "and", "as", "assert", "async", "await", "break", "class", "continue", "def",
    "del", "elif", "else", "except", "False", "finally", "for", "from", "global",
    "if", "import", "in", "is", "lambda", "None", "nonlocal", "not", "or",
    "pass", "raise", "return", "True", "try", "while", "with", "yield"
  ]

-- | Map a set of names to be acceptible for use in Python.
--
-- Names are mangled when they are Python keywords.
mangleNames :: Set Name -> Map Name Name
mangleNames names = M.fromSet mangle names
    where
        suffix :: Name -> Name
        suffix (Name t) =
            let n' = Name (t <> "_")
            in if n' `elem` names then suffix n' else n'
        mangle :: Name -> Name
        mangle n@(Name t)
            | t `elem` pythonKeywords = suffix n
            | otherwise = n

-- -----------------------------------------------------------------------------
