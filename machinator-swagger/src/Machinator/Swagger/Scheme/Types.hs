{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Machinator.Swagger.Scheme.Types where

import           Control.Lens ((.~), (?~), at)
import           Control.Lens.Tuple

import           Data.OpenApi
import           Data.OpenApi.Declare

import qualified GHC.Exts

import           Machinator.Core
import           Machinator.Swagger.Scheme.Types.Codegen

import           P
import           System.FilePath (FilePath)

fullSwagger :: [DefinitionFile] -> OpenApi
fullSwagger definitionFiles =
    spec
      & components . schemas .~ defs
      & info .~ info'
  where
    (defs, spec) = runDeclare (declareAnamlSwagger definitionFiles) mempty

    info' =
      mempty
        & title .~ "Anaml"


declareAnamlSwagger :: [DefinitionFile] -> Declare (Definitions Schema) OpenApi
declareAnamlSwagger defs = do
  declareFromDFs defs
  return
    $ mempty
    & paths .~
        GHC.Exts.fromList (join [
          makeCrud versionGroups "table" "TableId" "Table" "TableCreationRequest"
        , makeCrud versionGroups "entity" "EntityId" "Entity" "EntityCreationRequest"
        , makeCrud versionGroups "entity-mapping" "EntityMappingId" "EntityMapping" "EntityMappingCreationRequest"
        , makeCrud versionGroups "entity-population" "EntityPopulationId" "EntityPopulation" "EntityPopulationCreationRequest"
        , makeCrud versionGroups "feature" "FeatureId" "Feature" "FeatureCreationRequest"
        , makeCrud versionGroups "feature-set" "FeatureSetId" "FeatureSet" "FeatureSetCreationRequest"

        , makeCrud [] "event-store" "EventStoreId" "EventStore" "EventStoreCreationRequest"
        , makeCrud [] "feature-store" "FeatureStoreId" "FeatureStore" "FeatureStoreCreationRequest"
        , makeCrud [] "source" "SourceId" "Source" "SourceCreationRequest"
        , makeCrud [] "cluster" "ClusterId" "Cluster" "ClusterCreationRequest"
        , makeCrud [] "destination" "DestinationId" "Destination" "DestinationCreationRequest"

        , makeCrud' [] "user" "UserId" "User" "User" "UserCreationRequest" "UserUpdateRequestWithRoles"
        , makeCrud [] "user-group" "UserGroupId" "UserGroup" "UserGroupCreationRequest"

        , makeCrud' [] "branch" "BranchName" "BranchName" "Commit" "BranchRequest" "BranchUpdateRequest"
        , makeCrud [] "branch-protection" "BranchProtectionId" "BranchProtection" "BranchProtectionCreationRequest"
        , makeCrud [] "webhook" "WebhookId" "Webhook" "WebhookCreationRequest"

        , makeCrud [] "merge-request" "MergeRequestId" "MergeRequest" "MergeRequestCreationRequest"
            & traverse . _2 . put .~ Nothing
            & traverse . _2 . delete .~ Nothing

        , allowedPaths
        , commitPaths
        , checkPaths
        , onlineStorePaths
        ])


allowedPaths :: [(FilePath, PathItem)]
allowedPaths =
  [ ("/api/allowed-attribute", ) $
      mempty
        & get ?~ (mempty
          & at 200 ?~ getResponse "AllowedAttributesResponse")
  , ("/api/allowed-label",) $
      mempty
        & get ?~ (mempty
          & at 200 ?~ getResponse "AllowedLabelsResponse")
  ]


commitPaths :: [(FilePath, PathItem)]
commitPaths =
  [ ("/api/commit/{id}", ) $
      mempty
        & parameters .~ cid
        & get ?~ (mempty
          & at 200 ?~ getResponse "Commit")
  , ("/api/commit/{id}/history",) $
      mempty
        & parameters .~ cid
        & get ?~ (mempty
          & at 200 ?~ getListResponse "Commit")
  ]
 where
   cid =
    [ Inline $ mempty
      & name .~ "id"
      & required ?~ True
      & in_ .~ ParamPath
      & schema ?~ getParam "CommitId"
    ]

onlineStorePaths :: [(FilePath, PathItem)]
onlineStorePaths =
  [ ("/api/online-store/{id}/{name}/{entityId}", ) $
      mempty
        & parameters .~ params
        & get ?~ (mempty
          & at 200 ?~ getResponse "GeneratedFeatures")
  ]
 where
   params =
    [ Inline $ mempty
        & name .~ "id"
        & required ?~ True
        & in_ .~ ParamPath
        & schema ?~ getParam "FeatureStoreId"
    , Inline $ mempty
        & name .~ "name"
        & required ?~ True
        & in_ .~ ParamPath
        & schema ?~ string
    , Inline $ mempty
        & name .~ "entityId"
        & required ?~ True
        & in_ .~ ParamPath
        & schema ?~ getParam "EntityId"
    ]

checkPaths :: [(FilePath, PathItem)]
checkPaths =
  makeCrud [checkParam] "checks/{commit}" "CheckId" "Check" "UserGroupCreationRequest"
    where
  checkParam =
    Inline $ mempty
      & name .~ "commit"
      & required ?~ True
      & in_ .~ ParamPath
      & schema ?~ getParam "CommitId"

declareFromDFs :: [DefinitionFile] -> Declare (Definitions Schema) ()
declareFromDFs defs = do
  traverse_ declareFromDf defs

declareFromDf :: DefinitionFile -> Declare (Definitions Schema) ()
declareFromDf (DefinitionFile _ defs) =
  traverse_ genTypesV1 defs



getParam :: Text -> Referenced Schema
getParam =
  Ref . Reference

makeCrud :: [Referenced Param] -> FilePath -> Text -> Text -> Text -> [(FilePath, PathItem)]
makeCrud specGroups base idSchema readSchema createSchema =
  makeCrud' specGroups base idSchema readSchema readSchema createSchema createSchema

makeCrud' :: [Referenced Param] -> FilePath -> Text -> Text -> Text -> Text -> Text -> [(FilePath, PathItem)]
makeCrud' specGroups base idSchema listSchema readSchema createSchema updateSchema =
  [ ("/api/" <> base,
      mempty
        & parameters .~ specGroups
        & get ?~ (mempty
          & at 200 ?~ getListResponse listSchema)
        & post ?~ (mempty
          & requestBody ?~ getRequest createSchema
          & at 201 ?~ idResponse)
      )
  , ("/api/" <> base <> "/" <> "{id}",
      mempty
        & parameters .~ (<>) specGroups
          [ Inline $ mempty
              & name .~ "id"
              & required ?~ True
              & in_ .~ ParamPath
              & schema ?~ getParam idSchema
          ]
        & get ?~ (mempty
          & at 200 ?~ getResponse readSchema)
        & put ?~ (mempty
          & requestBody ?~ getRequest updateSchema
          & at 200 ?~ unitResponse)
        & delete ?~ (mempty
          & at 204 ?~ unitResponse)
      )
  ]

makeGets :: [Referenced Param] -> FilePath -> Text -> Text -> [(FilePath, PathItem)]
makeGets specGroups base idSchema readSchema =
  [ ("/api/" <> base,
      mempty
        & parameters .~ specGroups
        & get ?~ (mempty
          & at 200 ?~ getListResponse readSchema)
      )
  , ("/api/" <> base <> "/" <> "{id}",
      mempty
        & parameters .~ (<>) specGroups
          [ Inline $ mempty
              & name .~ "id"
              & required ?~ True
              & in_ .~ ParamPath
              & schema ?~ getParam idSchema
          ]
        & get ?~ (mempty
          & at 200 ?~ getResponse readSchema)
      )
  ]


versionGroups :: [Referenced Param]
versionGroups =
  [ Inline $ mempty
      & name .~ "branch"
      & required ?~ False
      & in_ .~ ParamQuery
      & schema ?~ string
  , Inline $ mempty
      & name .~ "commit"
      & required ?~ False
      & in_ .~ ParamQuery
      & schema ?~ getParam "CommitId"
  ]


getListResponse :: Text -> Referenced Response
getListResponse n =
  Inline $
    mempty
      & content.at "application/json" ?~ (mempty & schema ?~
        Inline (mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Ref (Reference n)))
        )

getResponse :: Text -> Referenced Response
getResponse n =
  Inline $
    mempty
      & content.at "application/json" ?~ (mempty & schema ?~ Ref (Reference n))


getRequest :: Text -> Referenced RequestBody
getRequest n =
  Inline $
    mempty
      & required ?~ True
      & content.at "application/json" ?~ (mempty & schema ?~ Ref (Reference n))

unitResponse :: Referenced Response
unitResponse =
  Inline $
    mempty
      & content.at "application/json" ?~ (mempty & schema ?~ Inline (mempty & type_ ?~ OpenApiObject))

idResponse :: Referenced Response
idResponse =
  Inline $
    mempty
      & content.at "application/json" ?~ (mempty & schema ?~ Inline (mempty & type_ ?~ OpenApiInteger))

string :: Referenced Schema
string =
  Inline $
    mempty & type_ ?~ OpenApiString
