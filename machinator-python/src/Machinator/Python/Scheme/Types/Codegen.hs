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
                             : fmap (\(m, md, fs) -> dataclass m (Just name) md fs) (c1:cts)

    Record fts          -> dataclass name Nothing mDoc fts

    Newtype (_, t)      ->
      -- TODO: We may want to do something a little more useful here.
      WL.vsep [
        "",
        "",
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
  in (:[]) . WL.group $
    open <> text trimmed <> arguments flds WL.<#> close


arguments :: [(Name, Type)] -> Doc a
arguments [] = mempty
arguments flds = WL.line <> "Args:" WL.<#> WL.indent 4 (
    WL.vsep $ fmap (\(Name n, ty) -> text n <> " (" <> genTypeV1 ty <> "): A data field") flds
  )

enumDocstring :: Maybe Docs -> [Doc a]
enumDocstring Nothing = []
enumDocstring (Just (Docs docs)) =
  let
    open  = string "\"\"\""
    close = string "\"\"\""
    trimmed = text . T.strip <$> T.lines docs
  in [WL.group $ open <> WL.vsep trimmed <> close, WL.mempty]


-- | Generates a Python enumeration.
enum :: Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
enum (Name n) mDoc ctors =
    WL.vsep [
      "",
      "",
      string "class" WL.<+> text n WL.<> WL.parens (string "enum.Enum") WL.<> ":",
      WL.indent 4 . WL.vsep $ enumDocstring mDoc <> fmap (\(Name m, _, []) -> WL.hsep [text m, WL.char '=', WL.dquotes $ text m]) ctors
    ]


-- | Generates a Python dataclass.
dataclass :: Name -> Maybe Name -> Maybe Docs -> [(Name, Type)] -> Doc a
dataclass name@(Name n) super mDoc flds =
  WL.linebreak WL.<#>
  WL.vsep
    [ string "@dataclasses.dataclass(frozen=True)"
    , string "class" <+> text n <> extends <> string ":"
    , WL.indent 4 . WL.vsep $ (
        classDocstring mDoc flds <>
        [""] <>
        fields flds <>
        serde name flds
      )
    ]
  where
    extends =
      case super of
        Nothing -> WL.mempty
        Just (Name sn) -> WL.parens $ text sn

fields :: [(Name, Type)] -> [Doc a]
fields = fmap (\(Name n, t) -> text n WL.<> WL.char ':' WL.<+> genTypeV1 t)

serde :: Name -> [(Name, Type)] -> [Doc a]
serde n flds =
  [ WL.mempty
  , generateJsonSchema n flds
  , WL.mempty
  , generateFromJson n flds
  , WL.mempty
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
    ]

arg :: (Name, Either Type Text) -> Doc a
arg (Name n, t) = text n WL.<> WL.char ':' WL.<+> either genTypeV1 text t

isEnum :: [(Name, Maybe Docs, [(Name, Type)])] -> Bool
isEnum [] = False
isEnum cs = go cs
  where
    go [] = True
    go ((_, _, []):rs) = go rs
    go _ = False

generateJsonSchema :: Name -> [(Name, Type)] -> Doc a
generateJsonSchema (Name n) flds =
    classmethod . method "json_schema" [] $ WL.vsep [
      "\"\"\"Return the JSON schema for " <> text n <> " data." WL.<#>
      WL.line <>
      "Returns:" WL.<#>
      WL.indent 4 ("The JSON schema to validate serialised " <> text n <> " values.") WL.<#>
      "\"\"\"",
      "return dict" <> WL.parens (
        WL.line <>
        WL.indent 4 (WL.vsep
          [ "type=\"object\","
          , "properties=dict(" WL.<#> WL.indent 4 (WL.vsep (fmap fieldSchema flds)) WL.<#> "),"
          , "required=" <> WL.encloseSep WL.lbracket WL.rbracket WL.comma (flds >>= requiredField)
        ]) <>
        WL.line
      )
    ]
  where
    fieldSchema (Name f, ty) =
      text f <> "=" <> schemaType ty <> ","
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
    MaybeT ty' -> "dict(oneOf=[" <>
      "dict(type=\"null\"), " <> 
      schemaType ty'
     <> "])"

generateToJson :: Name -> [(Name, Type)] -> Doc a
generateToJson _ flds =
    method "to_json" [] $ WL.vsep [
      "\"\"\"Generate dictionary ready to be serialised to JSON." WL.<#>
      WL.line <>
      "Returns:" WL.<#>
      WL.indent 4 "A dictionary ready to serialised as JSON." WL.<#>
      "\"\"\""
      ,
      text "return dict" <> WL.parens (
        WL.line <>
        WL.indent 4 (WL.vsep (fmap serialiseField flds)) <>
        WL.line
      )
    ]
  where
    serialiseField (Name f, ty) =
      text f <> "=" <> serialiseType ("self." <> text f) ty <> ","

generateFromJson :: Name -> [(Name, Type)] -> Doc a
generateFromJson (Name n) flds =
    classmethod . method "from_json" [(Name "data", Right "dict")] $ WL.vsep [
      "\"\"\"" <> "Validate and parse JSON data into an instance of " <> text n <> "." WL.<#>
      WL.line <>
      "Args:" WL.<#>
      WL.indent 4 "data (dict): JSON dictionary to validate and parse." WL.<#>
      WL.line <>
      "Returns:" WL.<#>
      WL.indent 4 "An instance of " <> text n <> "." WL.<#>
      "\"\"\"",
      "try:",
      WL.indent 4 . WL.vsep $ [
        "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
          "data",
          "self.json_schema()"
        ],
        text "return" WL.<+> text n <> WL.parens
          (WL.line <> (WL.indent 4 . WL.vsep $ fmap parseField flds) <> WL.line)
      ],
      "except jsonschema.exceptions.ValidationError as ex:",
      WL.indent 4 . WL.vsep $ [
        "logging.debug(\"Invalid JSON data received while parsing " <> text n <> "\", exc_info=ex)",
        "raise"
      ]
    ]
  where
    parseField (Name f, ty) =
      text f <> "=" <> parseType ("data.get(\"" <> f <> "\")") ty <> ","

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
