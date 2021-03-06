{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Pretty (
    ppDefinitionFile
  , ppDefinition
  , ppDefinitionAnnotated
  , SyntaxAnnotation (..)
  ) where


import           Data.Functor.Identity (Identity(..), runIdentity)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Machinator.Core.Data.Definition
import           Machinator.Core.Data.Version

import           P

import           Text.PrettyPrint.Annotated.Leijen (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.Leijen as WL


ppDefinitionFile :: Versioned DefinitionFile -> Text
ppDefinitionFile (Versioned v df) =
  prettyUndecorated (ppDefinitionFile' v df)

ppDefinition :: Definition -> Text
ppDefinition =
  prettyUndecorated . ppDefinition'

data SyntaxAnnotation =
    Punctuation
  | Keyword
  | Primitive
  | TypeDefinition Text
  | TypeUsage Text
  | ConstructorDefinition Text
  | FieldDefinition Text Text
  | VersionMarker
  deriving (Eq, Ord, Show)

ppDefinitionAnnotated ::
     (SyntaxAnnotation -> Text)
  -> (SyntaxAnnotation -> Text)
  -> Definition
  -> Text
ppDefinitionAnnotated start end =
  prettyDecorated start end . ppDefinition'

-- -----------------------------------------------------------------------------

ppDefinitionFile' :: MachinatorVersion -> DefinitionFile -> Doc SyntaxAnnotation
ppDefinitionFile' v (DefinitionFile _ defs) =
          ppVersion v
  WL.<$$> cat (WL.punctuate (WL.linebreak WL.<> WL.linebreak) (fmap ppDefinition' defs))

ppVersion :: MachinatorVersion -> Doc SyntaxAnnotation
ppVersion v =
  WL.annotate VersionMarker $
    text "-- machinator @ v" WL.<> WL.int (versionToNumber v)

ppDefinition' :: Definition -> Doc SyntaxAnnotation
ppDefinition' (Definition n _ ty) =
  case ty of
    Variant cs ->
      ppVariant n cs
    Record fts ->
      ppRecord n fts
    Newtype ft ->
      ppNewtype n ft

ppVariant :: Name -> NonEmpty (Name, Maybe Docs, [(Name, Type)]) -> Doc SyntaxAnnotation
ppVariant (Name n) cs =
  WL.hang
    2
    (keyword "data" <+>
     WL.annotate (TypeDefinition n) (text n) <+>
     punctuation "=" WL.<$$>
     foldl'
       (<+>)
       (text " ")
       (WL.punctuate
          (WL.linebreak WL.<> punctuation "|")
          (NE.toList (fmap (uncurry3 ppConstructor) cs))))

ppConstructor :: Name -> Maybe Docs -> [(Name, Type)] -> Doc SyntaxAnnotation
ppConstructor nn@(Name n) _ ts =
  WL.hang
    2
    (WL.annotate (ConstructorDefinition n) (ppName nn) WL.<>
     foldl' (<+>) WL.empty (fmap (uncurry (ppRecordField nn)) ts))

ppRecord :: Name -> [(Name, Type)] -> Doc SyntaxAnnotation
ppRecord nn@(Name n) fts =
  WL.hang
    2
    (keyword "record" <+>
     WL.annotate (TypeDefinition n) (ppName nn) <+>
     punctuation "=" <+>
     punctuation "{" WL.<$$>
     foldl'
       (<+>)
       (text " ")
       (WL.punctuate
          (WL.linebreak WL.<> punctuation ",")
          (fmap (uncurry (ppRecordField nn)) fts)) WL.<$$>
     punctuation "}")

ppNewtype :: Name -> (Name, Type) -> Doc SyntaxAnnotation
ppNewtype nn@(Name n) (fn, ft) =
  WL.hang
    2
    (keyword "newtype" <+>
     WL.annotate (TypeDefinition n) (ppName nn) <+>
     punctuation "=" <+>
     punctuation "{" WL.<$$>
     ppRecordField nn fn ft WL.<$$>
     punctuation "}")


ppRecordField :: Name -> Name -> Type -> Doc SyntaxAnnotation
ppRecordField (Name tn) nn@(Name n) ty =
  WL.annotate (FieldDefinition tn n) (ppName nn) <+> punctuation ":" <+> ppType 0 ty

ppType :: Int -> Type -> Doc SyntaxAnnotation
ppType p t =
  case t of
    Variable nn@(Name n) ->
      WL.annotate (TypeUsage n) (ppName nn)
    GroundT g ->
      ppGroundType g
    ListT lt ->
      if p > 10 then
        punctuation "(" WL.<> primitive "List" <+> ppType 11 lt WL.<> punctuation ")"
      else
        primitive "List" <+> ppType 11 lt
    NonEmptyT lt ->
      if p > 10 then
        punctuation "(" WL.<> primitive "NonEmpty" <+> ppType 11 lt WL.<> punctuation ")"
      else
        primitive "NonEmpty" <+> ppType 11 lt
    MaybeT lt ->
      if p > 10 then
        punctuation "(" WL.<> primitive "Maybe" <+> ppType 11 lt WL.<> punctuation ")"
      else
        primitive "Maybe" <+> ppType 11 lt
    MapT k v ->
      if p > 10 then
        punctuation "(" WL.<> primitive "Map" <+> ppType 11 k <+> ppType 11 v WL.<> punctuation ")"
      else
        primitive "Map" <+> ppType 11 k <+> ppType 11 v

ppGroundType :: Ground -> Doc SyntaxAnnotation
ppGroundType =
  primitive . unName . groundToName

ppName :: Name -> Doc a
ppName =
  text . unName

-- -----------------------------------------------------------------------------

text :: Text -> Doc a
text =
  WL.string . T.unpack

pretty :: Doc a -> WL.SimpleDoc a
pretty =
  WL.renderPretty 0.4 80

cat :: [Doc a] -> Doc a
cat =
  foldl' (WL.<>) WL.empty

punctuation :: Text -> Doc SyntaxAnnotation
punctuation =
  WL.annotate Punctuation . text

keyword :: Text -> Doc SyntaxAnnotation
keyword =
  WL.annotate Keyword . text

primitive :: Text -> Doc SyntaxAnnotation
primitive =
  WL.annotate Primitive . text

prettyDecorated :: (a -> Text) -> (a -> Text) -> Doc a -> Text
prettyDecorated start end =
  runIdentity . WL.displayDecoratedA str (pure . start) (pure . end) . pretty
  where
    str :: [Char] -> Identity Text
    str = pure . T.pack

prettyUndecorated :: Doc a -> Text
prettyUndecorated =
  prettyDecorated (const mempty) (const mempty)

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
