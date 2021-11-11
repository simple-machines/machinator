{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Scheme.Types.Codegen (
    genTypesV1
  , genTypeV1
  , genImportsV1
  ) where


import           Data.Foldable (foldl1)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL


-- | Generates import statements.
genImportsV1 :: Name -> Set Name -> Text
genImportsV1 (Name n) ns =
    renderText $ "import" <+> text n <> "." <> imports
  where
    names = WL.punctuate "," (fmap (text . unName) (S.toAscList ns))
    imports | S.size ns == 1 = WL.hsep names
            | otherwise = WL.group (WL.flatAlt ("{" WL.<##> WL.indent 4 (WL.fillSep names) WL.<##> "}") (WL.braces (WL.hsep names)))

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
genTypesV1' (Definition name _ dec) =
  case dec of
    Variant ctors@(c1 :| cts) ->
      WL.vsep $
          genVariantV1 name Nothing (variantProperties ctors)
        : fmap (uncurry3 (genConstructorV1 name)) (c1:cts)

    Record fts ->
      genRecordV1 False name fts

    Newtype ft ->
      genRecordV1 False name [ft] <+> text "extends" <+> text "AnyVal"

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
    MapT k v ->
      string "Map" <> WL.brackets (genTypeV1 k <> "," <+> genTypeV1 v)

-- | Find the set of common properties in the constructors of a variant.
variantProperties :: NonEmpty (Name, Maybe Docs, [(Name, Type)]) -> [(Name, Type)]
variantProperties ctors =
  let fs = fmap (\(_, _, f) -> S.fromList f) ctors
  in S.toAscList (foldl1 S.intersection fs)

-- | Generate the sealed trait for a variant type.
genVariantV1 :: Name -> Maybe Docs -> [(Name, Type)] -> Doc a
genVariantV1 (Name n) mDoc fs =
  let
    field (Name f, ty) = "val" <+> text f <> ":" <+> genTypeV1 ty
    hd = "sealed trait" <+> text n
    built = case fs of
      [] -> hd
      _  -> hd <+> "{" WL.<##> WL.indent 4 (WL.vsep (fmap field fs)) WL.<##> "}"
  in case mDoc of
    Just (Docs docs) ->
      WL.vsep [ simpleComment docs, built ]
    Nothing ->
      built

-- | Generate the case class for a variant type data constructor.
genConstructorV1 :: Name -> Name -> Maybe Docs -> [(Name, Type)] -> Doc a
genConstructorV1 (Name extends) constructorName mDoc tys =
  let
    built = genRecordV1 True constructorName tys <+> text "extends" <+> text extends
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
genRecordV1 :: Bool -> Name -> [(Name, Type)] -> Doc a
genRecordV1 isConstructor (Name n) []
  | isConstructor
  = WL.hang 2 $
      text "case object" <+> text n
  | otherwise
  = WL.hang 2 $
      text "case class" <+> text n <> WL.tupled []

genRecordV1 _ (Name n) fts =
  WL.hang 2 $
    text "case class" <+> text n <> WL.tupled (
      with fts $ \(Name fn, ty) ->
        text fn <> string ":" <+> genTypeV1 ty
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
  let open  = "/**" <> WL.line
  let close = WL.line <> " */"
  let body = WL.pretty $ T.stripEnd (T.unlines (T.stripEnd . (" * " <>) . T.strip <$> T.lines docs))
  open <> WL.align body <> close


-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
