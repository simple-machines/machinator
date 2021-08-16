{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TupleSections     #-}
module Machinator.Swagger.Scheme.Types.Codegen (
    genTypesV1

  , genTypeV1
  ) where


import           Control.Lens ((?~), (.~))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import qualified Data.Text as Text

import           Data.OpenApi
import           Data.OpenApi.Declare

import qualified GHC.Exts as Exts

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Declare (Definitions Schema) (Referenced Schema)
genTypesV1 (Definition (Name parent) dec) =
  case dec of
    Variant cs -> do
      (discriminators, oneOfs) <-
        fmap List.unzip $
          for (NonEmpty.toList cs) $ \(Name constructor, fields) -> do
            ref <- makeRef constructor $
              genRecordV1 fields

            return ((makeDiscriminator parent constructor, makeRelativeRef constructor), ref)

      makeRef parent $
        mempty
          & type_ ?~ OpenApiObject
          & oneOf ?~ oneOfs
          & discriminator ?~ Discriminator "adt_type" (Exts.fromList discriminators)

    Record fts ->
      makeRef parent $
        genRecordV1 fts

genRecordV1 :: [(Name, Type)] -> Schema
genRecordV1 fts =
  let
    props =
      bimap unName genTypeV1 <$> fts

    reqs =
      [ fn | (Name fn, typ) <- fts, not (isMaybeT typ) ]
  in
    mempty
      & type_      ?~ OpenApiObject
      & properties .~ Exts.fromList props
      & required   .~ reqs



makeRef :: Text -> Schema -> Declare (Definitions Schema) (Referenced Schema)
makeRef n s = do
  declare $ Exts.fromList [(n,s)]
  return (Ref (Reference n))



genTypeV1 :: Type -> Referenced Schema
genTypeV1 ty =
  case ty of
    Variable (Name n) ->
      Ref (Reference n)
    GroundT g ->
      case g of
        StringT ->
          Inline $ paramSchemaToSchema (Proxy :: Proxy Text)
        BoolT ->
          Inline $ paramSchemaToSchema (Proxy :: Proxy Bool)
        IntT ->
          Inline $ paramSchemaToSchema (Proxy :: Proxy Int)
    ListT t2 -> do
      let ref = genTypeV1 t2
      Inline
        $ mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject ref

    MaybeT t2 ->
      genTypeV1 t2

isMaybeT :: Type -> Bool
isMaybeT ty =
  case ty of
    MaybeT _ -> True
    _ -> False



makeDiscriminator :: Text -> Text -> Text
makeDiscriminator parent constructor =
  Text.toLower $
    fromMaybe constructor $
      Text.stripSuffix parent constructor

makeRelativeRef :: Text -> Text
makeRelativeRef constructor =
  "#/components/schemas/" <> constructor


-- -----------------------------------------------------------------------------
