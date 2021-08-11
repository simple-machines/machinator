{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Scheme.Types.Codegen (
    genTypesV1

  , genTypeV1
  ) where


import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>), (<##>))
import qualified Text.PrettyPrint.Annotated.WL as WL


-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Text
genTypesV1 (Definition name@(Name n) dec) =
  renderText $ case dec of
    Variant (c1 :| cts) ->
      WL.vsep $
          string "sealed trait" <+> text n
        : fmap (uncurry (genConstructorV1 name)) (c1:cts)

    Record fts ->
      genRecordV1 name fts

genTypeV1 :: Type -> Doc a
genTypeV1 ty =
  case ty of
    Variable (Name n) ->
      text n
    GroundT g ->
      case g of
        StringT ->
          text "String"
        BoolT ->
          text "Boolean"
    ListT t2 ->
      string "List" <> WL.brackets (genTypeV1 t2)
    MaybeT t2 ->
      string "Option" <> WL.brackets (genTypeV1 t2)

genConstructorV1 :: Name -> Name -> [(Name, Type)] -> Doc a
genConstructorV1 (Name extends) constructorName tys =
  genRecordV1 constructorName tys <+> text "extends" <+> text extends

-- | Generates a naked record for the given definition.
--
-- @
-- [(Name "foo", GroundT StringT), (Name "bar", GroundT StringT)]
-- { foo :: String, bar :: String }
-- @
genRecordV1 :: Name -> [(Name, Type)] -> Doc a
genRecordV1 (Name n) fts =
  WL.hang 2 $
    text "case class" <+> text n <> WL.tupled (
      with fts $ \(Name n, ty) ->
        text n <+> string ":" <+> genTypeV1 ty
    )

-- -----------------------------------------------------------------------------

string :: [Char] -> Doc a
string =
  WL.text

text :: Text -> Doc a
text =
  WL.text . T.unpack

renderText :: Doc a -> Text
renderText =
  TL.toStrict . WL.displayT . WL.renderPrettyDefault
