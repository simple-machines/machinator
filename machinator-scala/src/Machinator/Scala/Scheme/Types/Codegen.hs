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

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL


-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Text
genTypesV1 def@(Definition _ mDoc _) =
  renderText $
    case mDoc of
      Just (Docs docs) ->
        WL.vsep [
          simpleComment docs
        , genTypesV1' def
        ]
      Nothing ->
        genTypesV1' def

-- | Generates a type declaration for the given definition.
genTypesV1' :: Definition -> Doc a
genTypesV1' (Definition name@(Name n) _ dec) =
  case dec of
    Variant (c1 :| cts) ->
      WL.vsep $
          string "sealed trait" <+> text n
        : fmap (uncurry3 (genConstructorV1 name)) (c1:cts)

    Record fts ->
      genRecordV1 name fts

    Newtype ft ->
      genRecordV1 name [ft] <+> text "extends" <+> text "AnyVal"

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
        IntT ->
          text "Int"
        LongT ->
          text "Long"
        DoubleT ->
          text "Double"
        UUIDT ->
          text "java.util.UUID"
        DateT ->
          text "java.time.LocalDate"
        DateTimeT ->
          text "java.time.Instant"
    ListT t2 ->
      string "List" <> WL.brackets (genTypeV1 t2)
    MaybeT t2 ->
      string "Option" <> WL.brackets (genTypeV1 t2)

genConstructorV1 :: Name -> Name -> Maybe Docs -> [(Name, Type)] -> Doc a
genConstructorV1 (Name extends) constructorName mDoc tys =
  let
    built = genRecordV1 constructorName tys <+> text "extends" <+> text extends
  in case mDoc of
    Just (Docs docs) ->
      WL.vsep [ simpleComment docs, built ]
    Nothing ->
      built

-- | Generates a naked record for the given definition.
--
-- @
-- [(Name "foo", GroundT StringT), (Name "bar", GroundT StringT)]
-- { foo :: String, bar :: String }
-- @
genRecordV1 :: Name -> [(Name, Type)] -> Doc a
genRecordV1 (Name n) [] =
  WL.hang 2 $
    text "case object" <+> text n

genRecordV1 (Name n) fts =
  WL.hang 2 $
    text "case class" <+> text n <> WL.tupled (
      with fts $ \(Name fn, ty) ->
        text fn <+> string ":" <+> genTypeV1 ty
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
  TL.toStrict . WL.displayT . WL.renderPretty 0.8 100


simpleComment :: Text -> Doc a
simpleComment docs = do
  let open  = WL.flatAlt "/* " "// "
  let close = WL.flatAlt (WL.line <> " */") ""
  let trimmed = T.stripEnd (T.unlines (T.strip <$> T.lines docs))
  WL.group $
    open <> WL.align (WL.pretty trimmed) <> close


-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
