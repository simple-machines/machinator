{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Scheme.Types where


import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Machinator.Core
import           Machinator.Core.Data.Definition
import qualified Machinator.Core.Graph as MG
import           Machinator.Python.Data.Types
import qualified Machinator.Python.Scheme.Types.Codegen as Codegen

import           P

import qualified System.FilePath.Posix as FilePath
import           System.IO (FilePath)


types :: PythonTypesVersion -> [DefinitionFile] -> Either PythonTypesError [(FilePath, Text)]
types v ds =
  case v of
    PythonTypesV1 ->
      typesV1 ds

typesV1 :: [DefinitionFile] -> Either PythonTypesError [(FilePath, Text)]
typesV1 dfs =
  let DefinitionFileGraph fg = MG.buildFileGraph dfs
      mg = M.mapKeys filePathToModuleName (fmap (M.mapKeys filePathToModuleName) fg)
  in for dfs $ \(DefinitionFile fp defs) ->
       let mn = filePathToModuleName fp in
       pure (genFileName mn, renderModule fp mn mg defs)

-- -----------------------------------------------------------------------------

renderModule :: FilePath -> ModuleName -> Map ModuleName (Map ModuleName (Set Name)) -> [Definition] -> Text
renderModule fp mn@(ModuleName n) imports defs =
  T.unlines (
    [ "\"\"\"Generated implementation of " <> n <> ".\"\"\""
    , ""
    , "# WARNING DO NOT EDIT"
    , "# This code was generated from " <> T.pack fp
    , ""
    , "from __future__ import annotations"
    , ""
    , "import dataclasses  # noqa: F401"
    , "import datetime  # noqa: F401"
    , "import enum  # noqa: F401"
    , "import logging  # noqa: F401"
    , "import uuid  # noqa: F401"
    , "import typing  # noqa: F401"
    , "import jsonschema  # noqa: F401"
    ]
    <> maybe mempty (("":) . fmap renderImport . M.toList) (mfilter (not . null) $ M.lookup mn imports)
    <> with defs Codegen.genTypesV1
  )

renderImport :: (ModuleName, Set Name) -> Text
renderImport (ModuleName n, ns) =
  let
    imports = (T.intercalate ", " . fmap unName . S.toAscList) ns
  in
    "from " <> n <> " import " <> imports


-- -----------------------------------------------------------------------------

newtype ModuleName = ModuleName {
    _unModuleName :: Text
  } deriving (Eq, Ord, Show)


-- | Derive a module name from the relative 'FilePath'.
--
-- @
-- λ> filePathToModuleName "./path_to/my/FavouriteTemplatePlace.mcn"
-- ModuleName {unModuleName = "path_to.my.favourite_template_place"}
-- @
--
-- TODO V2 Accept an alternative function as a parameter.
filePathToModuleName :: FilePath -> ModuleName
filePathToModuleName =
  ModuleName . T.pack . goLower . FilePath.dropExtension
  where
    -- Initial lower case
    goLower [] = []
    goLower (x:xs)
      | Char.isAlphaNum x = Char.toLower x : goSnake xs
      | otherwise = goSnake xs
    -- Subsequent snake case
    goSnake [] = []
    goSnake (x:xs)
      | x == '/'          = '.' : goLower xs
      | Char.isUpper x    = '_' : Char.toLower x : goSnake xs
      | Char.isAlphaNum x = x : goSnake xs
      | otherwise         = '_' : goSnake xs

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> ".py"
