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
      mg = M.mapKeys filePathToModuleName (fmap (S.map filePathToModuleName) fg)
  in for dfs $ \(DefinitionFile fp defs) ->
       let mn = filePathToModuleName fp in
       pure (genFileName mn, renderModule fp mn mg defs)

-- -----------------------------------------------------------------------------

renderModule :: FilePath -> ModuleName -> Map ModuleName (Set ModuleName) -> [Definition] -> Text
renderModule fp mn@(ModuleName n) imports defs =
  T.unlines [
      "\"\"\"" <> n <> "\n\nGenerated from: " <> T.pack fp <> "\n\"\"\""
    -- TODO: We should only include these when required for this module.
    , ""
    , "import datetime"
    , "import logging"
    , "import uuid"
    , ""
    , "import jsonschema"
    , ""
    , ""
    , "# Wot?"
    , "# " <> T.pack (show mn)
    , "# " <> T.pack (show (M.lookup mn imports))
    , maybe mempty (T.unlines . fmap renderImport . toList) (M.lookup mn imports)
    , ""
    , T.unlines . with defs $ \def ->
        Codegen.genTypesV1 def
    ]

renderImport :: ModuleName -> Text
renderImport (ModuleName n) =
  "from " <> n <> " import *"


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
  ModuleName . T.pack . FilePath.dropExtension
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
