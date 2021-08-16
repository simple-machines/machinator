{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Machinator.Swagger.Scheme.Types where

import           Control.Lens ((.~), (?~), at)

import           Data.OpenApi
import           Data.OpenApi.Declare

import           Machinator.Core
import           Machinator.Swagger.Scheme.Types.Codegen

import           P

fullSwagger :: [DefinitionFile] -> OpenApi
fullSwagger definitionFiles =
    spec
      & components . schemas .~ defs
      & info .~ info'
  where
    (defs, spec) = runDeclare (declareHackageSwagger definitionFiles) mempty

    info' =
      mempty
        & title   .~ "Anaml"
        & version .~ "0.1"

declareHackageSwagger :: [DefinitionFile] -> Declare (Definitions Schema) OpenApi
declareHackageSwagger defs = do
  declareFromDFs defs
  return
    $ mempty
    & paths .~
        [ ("/table", mempty & get ?~ (mempty
            & at 200 ?~ Inline (getResponse "Table")))
        , ("/table/{table-id}", mempty & get ?~ (mempty
            & parameters .~ [ Inline $ mempty
                & name .~ "table-id"
                & required ?~ True
                & in_ .~ ParamPath ]
            & at 200 ?~ Inline (getResponse "Table")))
        ]



declareFromDFs :: [DefinitionFile] -> Declare (Definitions Schema) ()
declareFromDFs defs = do
  traverse_ declareFromDf defs

declareFromDf :: DefinitionFile -> Declare (Definitions Schema) ()
declareFromDf (DefinitionFile _ defs) = do
  traverse_ genTypesV1 defs
  return mempty

getResponse :: Text -> Response
getResponse n =
  mempty
    & content.at "application/json" ?~ (mempty & schema ?~ Ref (Reference n))
