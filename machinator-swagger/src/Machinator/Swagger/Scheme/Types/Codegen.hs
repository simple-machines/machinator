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
import qualified Data.Text as Text

import           Data.OpenApi
import           Data.OpenApi.Declare

import qualified GHC.Exts as Exts

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Declare (Definitions Schema) (Referenced Schema)
genTypesV1 (Definition (Name parent) mDoc dec) =
  case dec of
    Variant cs -> do
      (discriminators, oneOfs) <-
        fmap List.unzip $
          for (NonEmpty.toList cs) $ \(Name constructor, vDoc, fields) -> do
            ref <- makeRef constructor $
              genRecordV1 vDoc fields

            return ((makeDiscriminator parent constructor, makeRelativeRef constructor), ref)

      makeRef parent $
        mempty
          & type_ ?~ OpenApiObject
          & oneOf ?~ oneOfs
          & discriminator ?~ Discriminator "adt_type" (Exts.fromList discriminators)
          & description .~ (docText <$> mDoc)

    Record fts ->
      makeRef parent $
        genRecordV1 mDoc fts

    Newtype (_, ft) ->
      makeRef parent $
        genNewtype mDoc ft

genRecordV1 :: Maybe Docs -> [(Name, Type)] -> Schema
genRecordV1 vDoc fts =
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
      & description .~ (docText <$> vDoc)

genNewtype :: Maybe Docs ->  Type -> Schema
genNewtype nDoc wrappedType =
  let
    openWrappedType =
      case wrappedType of
        var@Variable {} ->
          mempty
            & allOf ?~ [genTypeV1 var]
            & description .~ (docText <$> nDoc)
        GroundT g ->
          genGroundSchema g
            & description .~ (docText <$> nDoc)
        ListT t2 ->
          mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (genTypeV1 t2)
            & description .~ (docText <$> nDoc)
        NonEmptyT t2 ->
          mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (genTypeV1 t2)
            & minItems ?~ 1
            & description .~ (docText <$> nDoc)

        MaybeT t2 ->
          genNewtype nDoc t2
            & nullable ?~ True
        -- TODO: This just blindly assumes that the keys are serialised as JSON strings.
        MapT _ v ->
          mempty
            & type_ ?~ OpenApiObject
            & additionalProperties ?~ AdditionalPropertiesSchema (genTypeV1 v)
  in
    openWrappedType


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
      Inline $
        genGroundSchema g
    ListT t2 -> do
      Inline
        $ mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject (genTypeV1 t2)
    NonEmptyT t2 -> do
      Inline
        $ mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject (genTypeV1 t2)
        & minItems ?~ 1

    MaybeT t2 ->
      genTypeV1 t2

    -- TODO: This just blindly assumes that the keys are serialised as JSON strings.
    MapT _ v ->
      Inline
        $ mempty
        & type_ ?~ OpenApiObject
        & additionalProperties ?~ AdditionalPropertiesSchema (genTypeV1 v)

genGroundSchema :: Ground -> Schema
genGroundSchema g =
  case g of
    StringT ->
      mempty
        & type_ ?~ OpenApiString
    BoolT ->
      mempty
        & type_ ?~ OpenApiBoolean
    IntT ->
      mempty
        & type_ ?~ OpenApiInteger
        & format ?~ "int32"
    LongT ->
      mempty
        & type_ ?~ OpenApiInteger
        & format ?~ "int64"
    DoubleT ->
      mempty
        & type_ ?~ OpenApiNumber
        & format ?~ "double"
    UUIDT ->
      mempty
        & type_ ?~ OpenApiString
        & format ?~ "uuid"
    DateT ->
      mempty
        & description ?~ "A date as per RFC 3339"
        & format ?~ "date"
        & type_ ?~ OpenApiString
    DateTimeT ->
      mempty
        & description ?~ "A date time as per RFC 3339"
        & format ?~ "date-time"
        & type_ ?~ OpenApiString

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
