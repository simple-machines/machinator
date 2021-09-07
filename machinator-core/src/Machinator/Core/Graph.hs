{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Machinator.Core.Graph (
    buildFileGraph
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Machinator.Core.Data.Definition

import           P

import           System.IO (FilePath)


-- | Figures out the file graph
-- i.e. for each file, which other files does it depend on?
buildFileGraph :: [DefinitionFile] -> DefinitionFileGraph
buildFileGraph fs =
  let
    binds :: Map FilePath [Name]
    binds =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, fmap defName defs)

    uses :: Map FilePath [Name]
    uses =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, toList (fold (fmap (free . defType) defs)))

    inverted :: Map Name FilePath
    inverted =
      M.foldMapWithKey (\k ns -> foldl' (\acc v -> M.insertWith (<>) v k acc) mempty ns) binds

    fg :: Map FilePath (Map FilePath (Set Name))
    fg =
      M.fromList . with fs $ \(DefinitionFile fp _) ->
        let
          u :: Maybe [Name]
          u = M.lookup fp uses

          d :: [(FilePath, Set Name)]
          d = catMaybes (maybe mempty (fmap (\n -> fmap (, S.singleton n) (M.lookup n inverted))) u)

          i :: [Map FilePath (Set Name)]
          i = fmap (uncurry M.singleton) (filter ((/= fp) . fst) d)

          deps :: Map FilePath (Set Name)
          deps = foldr (M.unionWith (<>)) mempty i
        in (fp, deps)
  in DefinitionFileGraph fg
