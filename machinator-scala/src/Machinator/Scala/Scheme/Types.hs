{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Scheme.Types where


import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Text as T

import           Machinator.Core
import           Machinator.Core.Data.Definition
import qualified Machinator.Core.Graph as MG
import           Machinator.Scala.Data.Types
import qualified Machinator.Scala.Scheme.Circe.Codegen as Codegen
import qualified Machinator.Scala.Scheme.Types.Codegen as Codegen

import           P

import qualified System.FilePath.Posix as FilePath
import           System.IO (FilePath)


types :: ScalaTypesVersion -> Text -> [DefinitionFile] -> Either ScalaTypesError [(FilePath, Text)]
types v pkg ds =
  case v of
    ScalaTypesV1 ->
      typesV1 pkg ds

typesV1 :: Text -> [DefinitionFile] -> Either ScalaTypesError [(FilePath, Text)]
typesV1 pkg dfs =
  let DefinitionFileGraph fg = MG.buildFileGraph dfs
      mg = M.mapKeys (filePathToModuleName pkg) (fmap (M.mapKeys (filePathToModuleName pkg)) fg)
  in for dfs $ \(DefinitionFile fp defs) ->
       let mn = filePathToModuleName pkg fp in
       pure (genFileName mn, renderModule mn mg defs)

-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Map ModuleName (Map ModuleName (Set Name)) -> [Definition] -> Text
renderModule mn@(ModuleName n) imports defs =
  T.unlines [
      T.unwords ["package", n]
    , maybe mempty (T.unlines . fmap renderImports . M.toList) (mfilter (not . null) $ M.lookup mn imports)
    , T.unlines . with defs $ \def ->
        Codegen.genTypesV1 def <> "\n" <> Codegen.generateToJsonV1Companion def
    ]

renderImports :: (ModuleName, Set Name) -> Text
renderImports (ModuleName n, ns) = Codegen.genImportsV1 (Name n) ns

-- -----------------------------------------------------------------------------

newtype ModuleName = ModuleName {
    _unModuleName :: Text
  } deriving (Eq, Ord, Show)


-- | Derive a module name from the relative 'FilePath'.
--
-- @
-- λ> filePathToModuleName "example" "./path_to/my/favourite_Template_place.hs"
-- ModuleName {unModuleName = "example.pathTo.my.favouriteTemplatePlace"}
-- @
--
-- TODO V2 Accept an alternative function as a parameter.
filePathToModuleName :: Text -> FilePath -> ModuleName
filePathToModuleName pkg =
  ModuleName . (\m -> pkg <> "." <> m) . T.pack . goLower . FilePath.dropExtension
  where
    goLower [] = []
    goLower (x:xs)
      | Char.isAlphaNum x = Char.toLower x : go xs
      | otherwise = goLower xs
    go [] = []
    go (x:xs)
      | x == '/' = '.' : goLower xs
      | Char.isAlphaNum x = x : go xs
      | otherwise = goLower xs

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> "/generated.scala"
