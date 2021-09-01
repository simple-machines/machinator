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
genTypesV1 def = renderText $ genTypesV1' def

-- | Generates a type declaration for the given definition.
genTypesV1' :: Definition -> Doc a
genTypesV1' (Definition name@(Name n) mDoc dec) =
  case dec of
    Variant (c1 :| cts)
      | isEnum (c1:cts) -> enum name mDoc (c1:cts)
      | otherwise       -> WL.vsep $
                               dataclass name Nothing mDoc []
                             : fmap (uncurry3 (genConstructorV1 name)) (c1:cts)

    Record fts ->
      genRecordV1 name mDoc fts

    Newtype (_, t) ->
      -- TODO: We may want to do something a little more useful here.
      WL.vsep [
        comment mDoc,
        WL.hsep [text n, WL.char '=', genTypeV1 t]
      ]

genTypeV1 :: Type -> Doc a
genTypeV1 ty =
  case ty of
    Variable (Name n) ->
      -- NB: We don't actually do anything with typevars. This case will probably
      -- result in an error in the generated code.
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
genConstructorV1 (Name extends) name =
  dataclass name (Just extends)

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

genRecordV1 name mDoc fts = dataclass name Nothing mDoc fts

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

classDocstring :: Maybe Docs -> [(Name, Type)] -> [Doc a]
classDocstring Nothing _ = []
classDocstring (Just (Docs docs)) flds =
  let
    open  = string "\"\"\""
    close = string "\"\"\""
    trimmed = T.stripEnd (T.unlines (T.strip <$> T.lines docs))
    args = WL.vsep [
        WL.softbreak ,
        text "Args:",
        WL.indent 2 . WL.vsep $
          fmap (\(Name n, ty) -> text n WL.<+> WL.parens (genTypeV1 ty) <> WL.char ':' WL.<+> string "A field") flds
      ]
  in (:[]) . WL.group $
    open <> WL.align (
      WL.pretty trimmed WL.<#> args
    ) WL.<#> close

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

-- | Generates a Python enumeration.
enum :: Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
enum (Name n) mDoc ctors =
    WL.vsep [
      string "class" WL.<+> text n WL.<> WL.parens (string "enum.Enum") WL.<> ":",
      WL.indent 4 . WL.vsep $ classDocstring mDoc [] <> fmap (\(Name m, _, []) -> WL.hsep [text m, WL.char '=', WL.dquotes $ text m]) ctors
    ] WL.<> WL.line


-- | Generates a Python dataclass.
dataclass :: Name -> Maybe Text -> Maybe Docs -> [(Name, Type)] -> Doc a
dataclass name@(Name n) super mDoc flds =
  WL.vsep [
    string "@dataclass(frozen=True)",
    string "class" <+> text n <> extends <> string ":",
    WL.indent 4 . WL.vsep $ (classDocstring mDoc flds <> fields flds <> serde name flds),
    WL.line
  ]
  where
    extends =
      case super of
        Nothing -> WL.mempty
        Just sn -> WL.parens $ text sn

fields :: [(Name, Type)] -> [Doc a]
fields = fmap (\(Name n, t) -> text n WL.<> WL.char ':' WL.<+> genTypeV1 t)

serde :: Name -> [(Name, Type)] -> [Doc a]
serde n flds =
  [ WL.mempty
  , generateJsonSchema flds
  , generateFromJson n flds
  , generateToJson n flds
  ]

classmethod :: Doc a -> Doc a
classmethod body =
    WL.vsep [
      string "@classmethod", body
    ]

method :: Text -> [(Name, Either Type Text)] -> Doc a -> Doc a
method name args body =
    WL.vsep [
      string "def" WL.<+> text name <> WL.encloseSep WL.lparen WL.rparen WL.comma (string "self" : fmap arg args) WL.<> WL.char ':'
    , WL.indent 4 body
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

generateJsonSchema :: [(Name, Type)] -> Doc a
generateJsonSchema flds =
    classmethod . method "json_schema" [] $ WL.vsep [
      tripleQuote "Return the JSON schema for this data type.",
      "return dict" <> WL.parens (
        WL.line <>
        WL.indent 4 (WL.vsep
          [ "type=\"object\""
          , "properties=dict(" WL.<#> WL.indent 4 (WL.vsep (fmap fieldSchema flds)) WL.<#> ")"
          , "required=" <> WL.encloseSep WL.lbracket WL.rbracket WL.comma (flds >>= requiredField)
        ]) <>
        WL.line
      )
    ]
  where
    fieldSchema (Name f, ty) =
      text f <> "=" <> schemaType ty
    requiredField (Name n, ty) =
      case ty of 
        MaybeT _ -> []
        _        -> [WL.dquotes (text n)]

schemaType :: Type -> Doc a
schemaType ty =
  case ty of
    Variable (Name v) -> text v <> ".json_schema()"
    GroundT gr -> case gr of
      StringT -> "dict(type=\"string\")"
      BoolT -> "dict(type=\"boolean\")"
      IntT -> "dict(type=\"integer\")"
      LongT -> "dict(type=\"integer\")"
      DoubleT -> "dict(type=\"float\")"
      UUIDT -> "dict(type=\"string\", format=\"uuid\")"
      DateT -> "dict(type=\"string\", format=\"date\")"
      DateTimeT -> "dict(type=\"string\", format=\"date-time\")"
    ListT ty' -> "dict(type=\"array\", item="<> schemaType ty' <> ")"
    MaybeT ty' -> text "optional" <> WL.parens (schemaType ty')

generateToJson :: Name -> [(Name, Type)] -> Doc a
generateToJson _ flds =
    method "to_json" [] $ WL.vsep [
      tripleQuote (text "Generate dictionary ready to be serialised to JSON."),
      text "return dict" <> WL.parens (
        WL.line <>
        WL.indent 4 (WL.vsep (fmap serialiseField flds)) <>
        WL.line
      )
    ]
  where
    serialiseField (Name f, ty) =
      text f <> "=" <> serialiseType ("self." <> text f) ty

generateFromJson :: Name -> [(Name, Type)] -> Doc a
generateFromJson (Name n) flds =
    classmethod . method "from_json" [(Name "data", Right "dict")] $ WL.vsep [
      tripleQuote (text "Validate and parse JSON data into an instance of " <> text n <> "."),
      "try:",
      WL.indent 4 . WL.vsep $ [
        "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
          "data",
          "self.json_schema()"
        ],
        text "return" WL.<+> text n <> WL.parens
          (WL.line <> (WL.indent 4 . WL.vsep $ fmap parseField flds) <> WL.line)
      ],
      "except ValidationError as ex:",
      WL.indent 4 . WL.vsep $ [
        "logging.debug(\"Invalid JSON data received while parsing " <> text n <> "\", exc_info=ex)",
        "raise"
      ]
    ]
  where
    parseField (Name f, ty) =
      text f <> "=" <> parseType ("data.get(\"" <> f <> "\")") ty

tripleQuote :: Doc a -> Doc a
tripleQuote s = text "\"\"\"" <> s <> text "\"\"\""

serialiseType :: Doc a -> Type -> Doc a
serialiseType value ty = case ty of
  Variable _ -> value <> ".to_json()"
  GroundT gr -> case gr of
    StringT -> value
    BoolT -> value
    IntT -> value
    LongT -> value
    DoubleT -> value
    UUIDT -> "str(" <> value <> ")"
    DateT -> value <> ".strfmt('%Y-%m-%d')"
    DateTimeT -> value <> ".strfmt('%Y-%m-%dT%H:%M:%S.%f%z')"
  ListT ty' -> "[" <> serialiseType "v" ty' <> " for v in " <> value <> "]"
  MaybeT ty' -> "(lambda v: " <> serialiseType "v" ty' <> " if v is not None else None)(" <> value <> ")"

parseType :: Text -> Type -> Doc a
parseType value ty = case ty of
  Variable (Name t) -> text t <> ".from_json(" <> text value <> ")"
  GroundT gr -> case gr of
    StringT -> "str(" <> text value <> ")"
    BoolT -> "bool(" <> text value <> ")" -- TODO: This is wrong
    IntT -> "int(" <> text value <> ")"
    LongT -> "int(" <> text value <> ")"
    DoubleT -> "float(" <> text value <> ")"
    UUIDT -> "UUID(hex=" <> text value <> ")"
    DateT -> "datetime.date()"
    DateTimeT -> "datetime.datetime()"
  ListT ty' -> "[" <> parseType "v" ty' <> " for v in " <> text value <> "]"
  MaybeT ty' -> "(lambda v: " <> parseType "v" ty' <> " if v is not None else None)(" <> text value <> ")"
