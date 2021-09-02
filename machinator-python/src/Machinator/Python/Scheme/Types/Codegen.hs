{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Scheme.Types.Codegen (
    genTypesV1
  , genTypeV1
  ) where


import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List (zipWith, tail)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL

import           Machinator.Python.Mangle


-- | Name of the JSON property and field.
discriminatorProperty :: Text
discriminatorProperty = "adt_type"


-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Text
genTypesV1 def = renderText $ genTypesV1' def


-- | Generates a type declaration for the given definition.
genTypesV1' :: Definition -> Doc a
genTypesV1' (Definition name mDoc dec) =
  case dec of
    Variant (c1 :| cts)
      | isEnum (c1:cts) -> enum name mDoc (c1:cts)
      | otherwise       -> WL.vsep $
                               superclass name mDoc
                             : fmap (\(m, md, fs) -> dataclass m (Just name) md fs) (c1:cts)

    Record fts          -> dataclass name Nothing mDoc fts

    Newtype field       -> wrapperclass name Nothing mDoc field


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
      text "typing.List" WL.<> WL.brackets (genTypeV1 t2)
    MaybeT t2 ->
      string "typing.Optional" <> WL.brackets (genTypeV1 t2)

-- -----------------------------------------------------------------------------

-- | Generate a Python docstring following Google's conventions.
googleDocstring
  :: Maybe Docs     -- ^ Documentation.
  -> [(Name, Type)] -- ^ Arguments
  -> Maybe Text     -- ^ Return value
  -> [(Name, Text)] -- ^ Exceptions
  -> [Doc a]
googleDocstring Nothing [] Nothing _ = []
googleDocstring (Just (Docs docs)) [] Nothing _ = ["\"\"\"" WL.<> text (T.strip docs) WL.<> "\"\"\""]
googleDocstring mDocs flds mRet exc =
  let
    mangle = mangleNames . S.fromList $ fmap fst flds
    trimmed = case mDocs of
      Just (Docs docs) -> text . T.strip <$> T.lines docs
      Nothing -> mempty
    args (nm@(Name n), ty) = (M.findWithDefault nm nm mangle, ty, Docs ("The '" <> n <> "' field."))
    ret = case mRet of
      Nothing -> mempty
      Just t ->  [WL.line <> "Returns:" WL.<#> WL.indent 4 (text t)]
    exceptions = case exc of
      [] -> mempty
      _ -> [
          WL.line <> "Raises:" WL.<#> WL.indent 4 (
            WL.vsep $ fmap (\(Name n, d) -> text n <> ": " <> text d) exc
          )
        ]
  in
    [
      "\"\"\"" <> WL.vsep (trimmed <> arguments (fmap args flds) <> ret <> exceptions) WL.<#>
      "\"\"\""
    ]

arguments :: [(Name, Type, Docs)] -> [Doc a]
arguments [] = mempty
arguments flds = [
    WL.line <> "Args:" WL.<#> WL.indent 4 (
      WL.vsep $ fmap (\(Name n, ty, Docs d) -> text n <> " (" <> genTypeV1 ty <> "): " <> text d) flds
    )
  ]


-- | Predicate: Is a variant type an enumeration?
isEnum :: [(Name, Maybe Docs, [(Name, Type)])] -> Bool
isEnum [] = False
isEnum cs = go cs
  where
    go [] = True
    go ((_, _, []):rs) = go rs
    go _ = False


-- | Generates a Python enumeration.
enum :: Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
enum n@(Name klass) mDoc ctors =
  WL.linebreak WL.<#>
  WL.vsep [
      string "class" WL.<+> text klass WL.<> WL.parens (string "enum.Enum") WL.<> ":",
      WL.indent 4 . WL.vsep $ (
        googleDocstring mDoc [] Nothing [] <>
        fmap (\(Name m, _, []) -> WL.hsep [text m, WL.char '=', WL.dquotes $ text m]) ctors <>
        enumserde n ctors
      )
  ]


superclass :: Name -> Maybe Docs -> Doc a
superclass name@(Name n) mDoc =
  WL.linebreak WL.<#>
  WL.vsep
    [ "@dataclasses.dataclass(frozen=True)"
    , "class" <+> text n <> ":"
    , WL.indent 4 . WL.vsep $ (
        googleDocstring mDoc [] Nothing [] <>
        [""] <>
        discriminator (Name "") <>
        superserde name
      )
    ]


-- | Generates a Python dataclass.
dataclass :: Name -> Maybe Name -> Maybe Docs -> [(Name, Type)] -> Doc a
dataclass name@(Name n) super mDoc fieldDefs =
  let
    extends =
      case super of
        Just (Name sn) -> WL.parens $ text sn
        Nothing        -> WL.mempty
  in
    WL.linebreak WL.<#>
    WL.vsep
      [ string "@dataclasses.dataclass(frozen=True)"
      , string "class" <+> text n <> extends <> string ":"
      , WL.indent 4 . WL.vsep $ (
          googleDocstring mDoc fieldDefs Nothing [] <>
          [""] <>
          discriminator name <>
          [""] <>
          fields fieldDefs <>
          serde name fieldDefs
        )
      ]


-- | Generates a Python wrapper dataclass.
--
-- This is our Python encoding of newtype wrappers.
wrapperclass :: Name -> Maybe Name -> Maybe Docs -> (Name, Type) -> Doc a
wrapperclass name@(Name n) super mDoc field =
  let
    extends =
      case super of
        Just (Name sn) -> WL.parens $ text sn
        Nothing        -> WL.mempty
  in
    WL.linebreak WL.<#>
    WL.vsep
      [ string "@dataclasses.dataclass(frozen=True)"
      , string "class" <+> text n <> extends <> string ":"
      , WL.indent 4 . WL.vsep $ (
          googleDocstring mDoc [field] Nothing [] <>
          [""] <>
          discriminator name <>
          [""] <>
          fields [field] <>
          wrapperserde name field
        )
      ]


-- | Generate fields to describe the discriminator.
discriminator :: Name -> [Doc a]
discriminator (Name klass) =
  [
    WL.vsep
      [ "ADT_TYPE" <> ":" <+> "typing.ClassVar" <> WL.brackets "str" <+> "=" <+> WL.dquotes (text klass)
      , "adt_type" <> ":" <+> "str" <+> "=" <+> "dataclasses.field(init=False, repr=False, default=ADT_TYPE)"
      ]
  ]


-- | Generates Python dataclass definitions.
--
-- Note that names are mangled according to Python lexical rules.
fields :: [(Name, Type)] -> [Doc a]
fields fs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fs
    field (nm, ty) = case M.findWithDefault nm nm mangle of
      Name n -> text n WL.<> WL.char ':' WL.<+> genTypeV1 ty
  in fmap field fs


enumserde :: Name -> [(Name, Maybe Docs, [(Name, Type)])] -> [Doc a]
enumserde n ctors =
  [ WL.mempty
  , generateEnumJsonSchema n (fmap (\(n, _, _) -> n) ctors)
  , WL.mempty
  , generateEnumFromJson n
  , WL.mempty
  , generateEnumToJson n
  ]

generateEnumJsonSchema :: Name -> [Name] -> Doc a
generateEnumJsonSchema (Name klass) ctors =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring
      (Just . Docs $ "JSON schema for enumeration " <> klass <> ".")
      []
      Nothing
      []
    <> [
      "return " <> dict [
        ("type", WL.dquotes "string"),
        ("enum", list $ fmap (WL.dquotes . text . unName) ctors)
      ]
    ]
  )

generateEnumFromJson :: Name -> Doc a
generateEnumFromJson (Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"))] -- "JSON dictionary to validate and parse."
        (Just $ "An instance of " <> klass <> ".")
        [
          (Name "ValidationError", "When schema validation fails."),
          (Name "KeyError", "When a required field is missing from the JSON.")
        ]
        <>
      [
        "try:",
        WL.indent 4 . WL.vsep $ [
          "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
            "data",
            "cls.json_schema()"
          ],
          text "return" WL.<+> text klass <> WL.parens ("str" <> WL.parens "data")
        ],
        "except jsonschema.exceptions.ValidationError as ex:",
        WL.indent 4 . WL.vsep $ [
          "logging.debug(\"Invalid JSON data received while parsing " <> text klass <> "\", exc_info=ex)",
          "raise"
        ]
      ]
    )

generateEnumToJson :: Name -> Doc a
generateEnumToJson _ =
  method "to_json" "self" [] $ WL.vsep (
    googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "A dictionary ready to serialise as JSON.") [] <> [
    text "return self.value"
  ])



-- | Generates JSON de-/serialise methods for a Python superclass.
superserde :: Name -> [Doc a]
superserde n =
  [ WL.mempty
  , generateSuperJsonSchema n
  , WL.mempty
  , generateSuperFromJson n
  ]


generateSuperJsonSchema :: Name -> Doc a
generateSuperJsonSchema (Name klass) =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring
      (Just . Docs $ "JSON schema for variant " <> klass <> ".")
      []
      Nothing
      []
    <> [
      "adt_types = [klass.ADT_TYPE for klass in cls.__subclasses__()]",
      "return " <> dict [
        ("type", WL.dquotes "object"),
        ("properties", dict [
          ("adt_type", dict [
            ("type", WL.dquotes "string"),
            ("enum", "adt_types")
          ])
        ])
      ]
    ]
  )


generateSuperFromJson :: Name -> Doc a
generateSuperFromJson (Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $
    WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"))]
        (Just $ "An instance of " <> klass <> ".")
        [
          (Name "ValidationError", "When schema validation fails."),
          (Name "KeyError", "When a required field is missing from the JSON.")
        ]
      <> [
        "try:",
        WL.indent 4 . WL.vsep $ [
          "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
            "data",
            "cls.json_schema()"
          ],
          "adt_type = data.get(" <> WL.dquotes (text discriminatorProperty) <> ", None)",
          "for klass in cls.__subclasses__():",
            WL.indent 4 (
              "if klass.ADT_TYPE == adt_type:" WL.<#>
                WL.indent 4 (
                  "return" <+> "klass.from_json(data)"
                )
            )
        ],
        "except jsonschema.exceptions.ValidationError as ex:",
        WL.indent 4 . WL.vsep $ [
          "logging.debug(\"Invalid JSON data received while parsing " <> text klass <> "\", exc_info=ex)",
          "raise"
        ]
      ]
    )


-- | Generates JSON de-/serialisation methods for a Python wrapper class.
wrapperserde :: Name -> (Name, Type) -> [Doc a]
wrapperserde n field =
  [ WL.mempty
  , generateWrapperJsonSchema n field
  , WL.mempty
  , generateWrapperFromJson n field
  , WL.mempty
  , generateWrapperToJson n field
  ]


generateWrapperJsonSchema :: Name -> (Name, Type) -> Doc a
generateWrapperJsonSchema (Name n) (Name _, t) =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring (Just . Docs $ "Return the JSON schema for " <> n <> " data.") [] (Just "JSON schema dictionary.") [] <> [
      "return" <+> schemaType t
    ]
  )


generateWrapperFromJson :: Name -> (Name, Type) -> Doc a
generateWrapperFromJson (Name klass) (Name f, ty) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"))] -- "JSON dictionary to validate and parse."
        (Just $ "An instance of " <> klass <> ".")
        [
          (Name "ValidationError", "When schema validation fails."),
          (Name "KeyError", "When a required field is missing from the JSON.")
        ]
        <>
      [
        "try:",
        WL.indent 4 . WL.vsep $ [
          "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
            "data",
            "cls.json_schema()"
          ],
          text "return" WL.<+> text klass <> WL.parens (parseType "data" ty)
        ],
        "except jsonschema.exceptions.ValidationError as ex:",
        WL.indent 4 . WL.vsep $ [
          "logging.debug(\"Invalid JSON data received while parsing " <> text klass <> "\", exc_info=ex)",
          "raise"
        ]
      ]
    )

-- | Generate the JSON serialisation method for a dataclass.
--
-- Note that names have been mangled according to Python lexical rules. See 'fields'
generateWrapperToJson :: Name -> (Name, Type) -> Doc a
generateWrapperToJson _ (f, ty) =
  let
    Name field = M.findWithDefault f f (mangleNames (S.singleton f))
  in
    method "to_json" "self" [] $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "A dictionary ready to serialise as JSON.") [] <> [
      text "return " <> serialiseType ("self." <> text field) ty
    ])


-- | Generates JSON de-/serialise methods for a Python dataclass.
serde :: Name -> [(Name, Type)] -> [Doc a]
serde n flds =
  [ WL.mempty
  , generateJsonSchema n flds
  , WL.mempty
  , generateFromJson n flds
  , WL.mempty
  , generateToJson n flds
  ]


-- | Generate the JSON schema method for a Python dataclass.
generateJsonSchema :: Name -> [(Name, Type)] -> Doc a
generateJsonSchema (Name n) flds =
    classmethod . method "json_schema" "cls" [] $ WL.vsep (
      googleDocstring (Just (Docs $ "Return the JSON schema for " <> n <> " data.")) [] (Just "JSON schema dictionary.") [] <> [
      "return" <+> dict [
        ("type", WL.dquotes "object"),
        ("properties", dict (
          (discriminatorProperty, dict [
            ("type", WL.dquotes "string"),
            ("enum", WL.list ["cls.ADT_TYPE"])
          ]) :
          fmap fieldSchema flds
        )),
        ("required", list $ WL.dquotes (text discriminatorProperty) : (flds >>= requiredField)
        )
      ]
    ])
  where
    fieldSchema :: (Name, Type) -> (Text, Doc a)
    fieldSchema (Name f, ty) = (f, schemaType ty)
    requiredField (Name f, ty) =
      case ty of
        MaybeT _ -> []
        _        -> [WL.dquotes (text f)]


-- | Generate the Python encoding of the JSON schema for a type.
schemaType :: Type -> Doc a
schemaType ty =
  case ty of
    Variable (Name v) -> text v <> ".json_schema()"
    GroundT gr -> case gr of
      StringT -> dict [("type", WL.dquotes "string")]
      BoolT -> dict [("type", WL.dquotes "boolean")]
      IntT -> dict [("type", WL.dquotes "integer")]
      LongT -> dict [("type", WL.dquotes "integer")]
      DoubleT -> dict [("type", WL.dquotes "float")]
      UUIDT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "uuid")]
      DateT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "date")]
      DateTimeT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "date-time")]
    ListT ty' -> "dict(type=\"array\", item="<> schemaType ty' <> ")"
    MaybeT ty' -> "dict(oneOf=[" WL.<#>
      WL.indent 4 (WL.vsep [
        "dict(type=\"null\"),",
        schemaType ty'
      ])
     WL.<#> "])"

-- | Generate the JSON serialisation method for a dataclass.
--
-- Note that names have been mangled according to Python lexical rules. See 'fields'
generateToJson :: Name -> [(Name, Type)] -> Doc a
generateToJson _ fieldDefs =
  let
    mangle = mangleNames (S.fromList (fmap fst fieldDefs))
    -- (fieldName, jsonProperty, type)
    properties = fmap (\(n, t) -> (M.findWithDefault n n mangle, n, t)) fieldDefs
    serialiseField (Name f, Name p, ty) =
      (p, serialiseType ("self." <> text f) ty)
  in
    method "to_json" "self" [] $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "A dictionary ready to serialise as JSON.") [] <> [
      text "return " <> dict (
        (discriminatorProperty, "self.ADT_TYPE") : fmap serialiseField properties
      )
    ])

generateFromJson :: Name -> [(Name, Type)] -> Doc a
generateFromJson (Name klass) fieldDefs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fieldDefs
    -- (fieldName, jsonProperty, type)
    properties = fmap (\(n, t) -> (M.findWithDefault n n mangle, n, t)) fieldDefs
    parseField (Name f, Name p, MaybeT ty) =
      text f <> "=" <> parseType ("data.get(\"" <> p <> "\", None)") ty <> ","
    parseField (Name f, Name p, ty) =
      text f <> "=" <> parseType ("data[\"" <> p <> "\"]") ty <> ","
  in
    classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"))] -- "JSON dictionary to validate and parse."
        (Just $ "An instance of " <> klass <> ".")
        [
          (Name "ValidationError", "When schema validation fails."),
          (Name "KeyError", "When a required field is missing from the JSON.")
        ]
        <>
      [
        "try:",
        WL.indent 4 . WL.vsep $ [
          "jsonschema.validate" <> WL.encloseSep WL.lparen WL.rparen WL.comma [
            "data",
            "cls.json_schema()"
          ],
          text "return" WL.<+> text klass <> WL.parens
            (WL.line <> (WL.indent 4 . WL.vsep $ fmap parseField properties) <> WL.line)
        ],
        "except jsonschema.exceptions.ValidationError as ex:",
        WL.indent 4 . WL.vsep $ [
          "logging.debug(\"Invalid JSON data received while parsing " <> text klass <> "\", exc_info=ex)",
          "raise"
        ]
      ]
    )

serialiseType :: Doc a -> Type -> Doc a
serialiseType value ty = case ty of
  Variable _ -> value <> ".to_json()"
  GroundT gr -> case gr of
    StringT -> "str" <> WL.parens value
    BoolT -> value
    IntT -> "int" <> WL.parens value
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

-- -----------------------------------------------------------------------------

classmethod :: Doc a -> Doc a
classmethod body =
  WL.vsep [
    string "@classmethod", body
  ]

method :: Text -> Text -> [(Name, Either Type Text)] -> Doc a -> Doc a
method name self args body =
  WL.vsep [
    string "def" WL.<+> text name <> WL.encloseSep WL.lparen WL.rparen WL.comma (text self : fmap arg args) WL.<> WL.char ':'
  , WL.indent 4 body
  ]

arg :: (Name, Either Type Text) -> Doc a
arg (Name n, t) =
  text n WL.<> WL.char ':' WL.<+> either genTypeV1 text t

-- | Literal Python dictionary.
dict :: [(Text, Doc a)] -> Doc a
dict [] = "{}"
dict fs =
    "{" WL.<#> WL.indent 4 (WL.vsep (fmap field fs)) WL.<#> "}"
  where
    field (n, v) = WL.dquotes (text n) <> ":" WL.<+> v <> ","

list :: [Doc a] -> Doc a
list [] = "[]"
list fs =
  let
    commas :: [Doc a]
    commas = tail (fmap (const ",") fs) <> [""]
    lst = zipWith (<>) fs commas
  in
    WL.brackets (WL.align $ WL.fillSep lst)

string :: [Char] -> Doc a
string =
  WL.text

text :: Text -> Doc a
text =
  WL.text . T.unpack

renderText :: Doc a -> Text
renderText =
  TL.toStrict . WL.displayT . WL.renderPretty 0.8 100
