{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Scheme.Types.Codegen (
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
  renderText $ genTypesV1' def

-- | Generates a type declaration for the given definition.
genTypesV1' :: Definition -> Doc a
genTypesV1' (Definition name@(Name n) mDoc dec) =
  case dec of
    Variant (c1 :| cts) 
      | isEnum (c1:cts) -> enum name mDoc (c1:cts)
      | otherwise       -> WL.vsep $
                               dataclass n Nothing mDoc []
                             : fmap (uncurry3 (genConstructorV1 name)) (c1:cts)

    Record fts ->
      genRecordV1 name mDoc fts

    Newtype ft@(_, t) ->
      WL.vsep [
        comment mDoc,
        WL.hsep [text n, WL.char '=', genTypeV1 t]
      ]

genTypeV1 :: Type -> Doc a
genTypeV1 ty =
  case ty of
    Variable (Name n) ->
      text n
    GroundT g ->
      case g of
        StringT ->
          text "str"
        BoolT ->
          text "bool"
        IntT ->
          text "int"
        LongT ->
          text "int"
        DoubleT ->
          text "float"
        UUIDT ->
          text "uuid.UUID"
        DateT ->
          text "datetime.date"
        DateTimeT ->
          text "datetime.datetime"
    ListT t2 ->
      text "List" WL.<> WL.brackets (genTypeV1 t2)
    MaybeT t2 ->
      string "Optional" <> WL.brackets (genTypeV1 t2)

genConstructorV1 :: Name -> Name -> Maybe Docs -> [(Name, Type)] -> Doc a
genConstructorV1 (Name extends) (Name constructorName) mDoc tys =
    dataclass constructorName (Just extends) mDoc tys

-- | Generates a naked record for the given definition.
--
-- @
-- [(Name "foo", GroundT StringT), (Name "bar", GroundT StringT)]
-- { foo :: String, bar :: String }
-- @
genRecordV1 :: Name -> Maybe Docs -> [(Name, Type)] -> Doc a
genRecordV1 (Name n) _ [] =
  WL.hang 2 $
    text "case object" <+> text n

genRecordV1 (Name n) mDoc fts = dataclass n Nothing mDoc fts

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

comment :: Maybe Docs -> Doc a
comment Nothing = WL.mempty
comment (Just (Docs docs)) = WL.vsep $ fmap (\t -> text "#" WL.<+> text (T.strip t)) $ T.lines docs

docstring :: Maybe Docs -> [Doc a]
docstring Nothing = []
docstring (Just (Docs docs)) =
  let
    open  = string "\"\"\""
    close = string "\"\"\""
    trimmed = T.stripEnd (T.unlines (T.strip <$> T.lines docs))
  in (:[]) . WL.group $
    open <> WL.align (WL.pretty trimmed) <> close

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

-- | Generates a Python enumeration.
enum :: Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
enum (Name n) mDoc ctors =
    WL.vsep [
      string "class" WL.<+> text n WL.<> WL.parens (string "enum.Enum") WL.<> ":",
      WL.indent 4 . WL.vsep $ docstring mDoc <> fmap (\(Name m, _, []) -> WL.hsep [text m, WL.char '=', WL.dquotes $ text m]) ctors
    ] WL.<> WL.line


-- | Generates a Python dataclass.
dataclass :: Text -> Maybe Text -> Maybe Docs -> [(Name, Type)] -> Doc a
dataclass name super mDoc flds = 
  WL.vsep [
    string "@dataclass(frozen=True)",
    string "class" <+> text name <> extends <> string ":",
    WL.indent 4 . WL.vsep $ (docstring mDoc <> fields flds <> serde name flds),
    WL.line
  ]
  where
    extends =
      case super of
        Nothing -> WL.mempty
        Just n -> WL.parens $ text n

fields :: [(Name, Type)] -> [Doc a]
fields = fmap (\(Name n, t) -> text n WL.<> WL.char ':' WL.<+> genTypeV1 t)

serde :: Text -> [(Name, Type)] -> [Doc a]
serde _ _ =
  [ WL.mempty
  , classmethod "json_schema" []
  , classmethod "from_json" [(Name "data", Right "dict")]
  , method "to_json" []
  ]

classmethod name args =
    WL.vsep [
      string "@classmethod"
    , string "def" WL.<+> text name WL.<> WL.encloseSep WL.lparen WL.rparen WL.comma (string "cls" : fmap arg args) WL.<> WL.char ':'
    , WL.indent 4 $ string "pass"
    ]  WL.<> WL.line

method name args =
    WL.vsep [
      string "def" WL.<+> text name <> WL.encloseSep WL.lparen WL.rparen WL.comma (string "self" : fmap arg args) WL.<> WL.char ':'
    , WL.indent 4 $ string "pass"
    ] WL.<> WL.line

arg :: (Name, Either Type Text) -> Doc a
arg (Name n, t) = text n WL.<> WL.char ':' WL.<+> either genTypeV1 text t

isEnum :: [(Name, Maybe Docs, [(Name, Type)])] -> Bool
isEnum [] = False
isEnum cs = go cs
  where
    go [] = True
    go ((_, _, []):rs) = go rs
    go _ = False

