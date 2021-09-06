{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Scheme.Types.Codegen
(
    genTypesV1
  , genTypeV1
  , genImportsV1
  )
where


import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import           Data.Set (Set)
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

genImportsV1 :: Name -> Set Name -> Text 
genImportsV1 (Name n) ns =
    renderText $
      "from" <+> ".." <> text n <+> "import" <+> imports
  where
    names = WL.punctuate "," (fmap (text . unName) (S.toAscList ns))
    imports = WL.group (WL.flatAlt ("(" WL.<##> WL.indent 4 (WL.fillSep names) WL.<##> ")") (WL.hsep names))

-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Text
genTypesV1 def = renderText $ genTypesV1' def


-- | Generates a type declaration for the given definition.
genTypesV1' :: Definition -> Doc a
genTypesV1' (Definition name mDoc dec) =
  case dec of
    Variant (c1 :| cts)
      | isEnum (c1:cts) -> enum name mDoc (fmap (\(n, _, _) -> n) $ c1:cts)
      | otherwise       -> WL.vsep $
                               variantclass name mDoc (c1:cts)
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

fieldDocstring :: (Name, Type) -> (Name, Type, Text)
fieldDocstring (n, ty) = (n, ty, "A data field.")


-- | Generate a Python docstring following Google's conventions.
googleDocstring
  :: Maybe Docs     -- ^ Documentation.
  -> [(Name, Type, Text)] -- ^ Arguments
  -> Maybe Text     -- ^ Return value
  -> [(Name, Text)] -- ^ Exceptions
  -> [Doc a]
googleDocstring Nothing [] Nothing _ = []
googleDocstring (Just (Docs d)) [] Nothing _ = ["\"\"\"" WL.<> text (T.strip d) WL.<> "\"\"\""]
googleDocstring mDocs flds mRet excs =
  let
    -- Mangle the field names.
    mangle = mangleNames . S.fromList $ fmap (\(a, _, _) -> a) flds

    description = case mDocs of
      Just (Docs d) -> [WL.vsep (text . T.strip <$> T.lines d)]
      Nothing -> mempty

    arg (nm, ty, comment) = text (unName (M.findWithDefault nm nm mangle)) <> " (" <> genTypeV1 ty <> "): " <> text comment
    argumentsSection =
      case flds of
        [] -> mempty
        _ -> ["Args:" WL.<#> WL.indent 4 (WL.vsep $ fmap arg flds)]

    returnsSection = case mRet of
      Nothing -> mempty
      Just t ->  ["Returns:" WL.<#> WL.indent 4 (text t)]

    exc (Name n, d) = text n <> ": " <> text d
    raisesSection =
      case excs of
        [] -> mempty
        _ -> ["Raises:" WL.<#> WL.indent 4 (WL.vsep $ fmap exc excs)]

    docs = description <> argumentsSection <> returnsSection <> raisesSection
  in
    case docs of
      []  -> mempty
      [d] -> ["\"\"\"" <> d <> "\"\"\""]
      ds  -> ["\"\"\"" <> WL.vcat (WL.punctuate WL.hardline ds) WL.<#> "\"\"\""]


-- | Generate fields to describe the discriminator.
discriminator :: Name -> [Doc a]
discriminator (Name klass) =
  [
    "",
    WL.vsep
      [ "ADT_TYPE" <> ":" <+> "typing.ClassVar" <> WL.brackets "str" <+> "=" <+> (WL.dquotes . text . T.toLower) klass
      , "adt_type" <> ":" <+> "str" <+> "=" <+> "dataclasses.field(init=False, repr=False, default=ADT_TYPE)"
      ]
  ]


-- | Generates Python dataclass definitions.
--
-- Note that names are mangled according to Python lexical rules.
fields :: [(Name, Type)] -> [Doc a]
fields [] = []
fields fs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fs
    field (nm, ty) = case M.findWithDefault nm nm mangle of
      Name n -> text n WL.<> WL.char ':' WL.<+> genTypeV1 ty
  in "" : fmap field fs

-- -----------------------------------------------------------------------------
-- $ Enumerations
--
-- Encode variants without fields as Python enumeration classes.

-- | Predicate: Is a variant type an enumeration?
isEnum :: [(Name, Maybe Docs, [(Name, Type)])] -> Bool
isEnum [] = False
isEnum cs = go cs
  where
    go [] = True
    go ((_, _, []):rs) = go rs
    go _ = False


-- | Generates a Python enumeration.
enum :: Name -> Maybe Docs -> [Name] -> Doc a
enum n@(Name klass) mDoc ctors =
  WL.linebreak WL.<#>
  WL.vsep [
      string "class" WL.<+> text klass WL.<> WL.parens (string "enum.Enum") WL.<> ":",
      WL.indent 4 . WL.vsep $ (
        googleDocstring mDoc [] Nothing [] <>
        fmap (\(Name m) -> WL.hsep [text m, WL.char '=', WL.dquotes $ text (T.toLower m)]) ctors <>
        serdeEnum n ctors
      )
  ]


-- | Generate the JSON de-/serialisation methods for an enumeration.
serdeEnum :: Name -> [Name] -> [Doc a]
serdeEnum n ctors =
  [ WL.mempty
  , generateJsonSchemaEnum n ctors
  , WL.mempty
  , generateFromJsonEnum n
  , WL.mempty
  , generateToJsonEnum
  ]


-- | Generate the JSON schema method for an enumeration.
generateJsonSchemaEnum :: Name -> [Name] -> Doc a
generateJsonSchemaEnum (Name klass) ctors =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring
      (Just . Docs $ "JSON schema for '" <> klass <> "'.")
      []
      (Just "A Python dictionary describing the JSON schema.")
      []
    <> [
      "return " <> dict [
        ("type", WL.dquotes "string"),
        ("enum", list $ fmap (WL.dquotes . text . T.toLower . unName) ctors)
      ]
    ]
  )


-- | Generate the JSON parsing method for an enumeration.
generateFromJsonEnum :: Name -> Doc a
generateFromJsonEnum (Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Left (GroundT StringT))] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", GroundT StringT, "JSON data to validate and parse.")]
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


-- | Generate the JSON serialisation method for an enumeration.
generateToJsonEnum :: Doc a
generateToJsonEnum =
  method "to_json" "self" [] $ WL.vsep (
    googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "JSON data ready to be serialised.") [] <> [
    text "return self.value"
  ])


-- -----------------------------------------------------------------------------
-- $ Wrapper classes
--
-- Encode a newtype declaration as a single-field dataclass.


-- | Generates a Python wrapper dataclass.
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
          googleDocstring mDoc [fieldDocstring field] Nothing [] <>
          discriminator name <>
          fields [field] <>
          serdeWrapper name field
        )
      ]


-- | Generates JSON de-/serialisation methods for a Python wrapper class.
serdeWrapper :: Name -> (Name, Type) -> [Doc a]
serdeWrapper n field@(_, ty) =
  [ WL.mempty
  , generateJsonSchemaWrapper n ty
  , WL.mempty
  , generateFromJsonWrapper n ty
  , WL.mempty
  , generateToJsonWrapper n field
  ]


-- | Generate the JSON schema method for a newtype wrapper.
generateJsonSchemaWrapper :: Name -> Type -> Doc a
generateJsonSchemaWrapper (Name n) t =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring
      (Just . Docs $ "Return the JSON schema for " <> n <> " data.")
      []
      (Just "A Python dictionary describing the JSON schema.")
      [] <>
    [
      "return" <+> schemaType t
    ]
  )


-- | Generate the JSON parsing method for a newtype wrapper.
generateFromJsonWrapper :: Name -> Type -> Doc a
generateFromJsonWrapper (Name klass) ty =
  classmethod . method "from_json" "cls" [(Name "data", Left ty)] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", ty, "JSON data to validate and parse.")]
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


-- | Generate the JSON serialisation method for newtype wrapper.
generateToJsonWrapper :: Name -> (Name, Type) -> Doc a
generateToJsonWrapper _ (f, ty) =
  let
    Name field = M.findWithDefault f f (mangleNames (S.singleton f))
  in
    method "to_json" "self" [] $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "Data ready to serialise as JSON.") [] <> [
      text "return " <> serialiseType ("self." <> text field) ty
    ])


-- -----------------------------------------------------------------------------
-- $ Variant classes
--
-- Encode variants as a superclass and a cluster of otherwise normal dataclasses
-- that extend it.


-- | Generate a wrapper class to encode a newtype.
variantclass :: Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
variantclass name@(Name n) mDoc ctors =
  let
    common = variantProperties ctors
  in
    WL.linebreak WL.<#>
    WL.vsep
      [ "@dataclasses.dataclass(frozen=True)"
      , "class" <+> text n <> "(abc.ABC):"
      , WL.indent 4 . WL.vsep $ (
          googleDocstring mDoc (fmap fieldDocstring common) Nothing [] <>
          discriminator (Name "") <>
          fields common <>
          serdeVariant name
        )
      ]


-- | Find the set of common properties in the constructors of a variant.
variantProperties :: [(Name, Maybe Docs, [(Name, Type)])] -> [(Name, Type)]
variantProperties ctors =
  let fs = fmap (\(_, _, f) -> S.fromList f) ctors
  in S.toAscList (foldr S.intersection (S.unions fs) fs)


-- | Generate JSON de-/serialise methods for a variant wrapperclass.
serdeVariant :: Name -> [Doc a]
serdeVariant n =
  [ WL.mempty
  , generateJsonSchemaVariant n
  , WL.mempty
  , generateFromJsonVariant n
  , WL.mempty
  , generateToJsonVariant n
  ]


-- | Generate the JSON schema method for a variant wrapperclass.
generateJsonSchemaVariant :: Name -> Doc a
generateJsonSchemaVariant (Name klass) =
  classmethod . method "json_schema" "cls" [] $ WL.vsep (
    googleDocstring
      (Just . Docs $ "JSON schema for variant " <> klass <> ".")
      []
      (Just "A Python dictionary describing the JSON schema.")
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


-- | Generate the JSON parsing method for a variant wrapperclass.
generateFromJsonVariant :: Name -> Doc a
generateFromJsonVariant (Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $
    WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"), "JSON data to validate and parse.")]
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


-- | Generate the JSON serialisation method for a dataclass.
generateToJsonVariant :: Name -> Doc a
generateToJsonVariant _ =
  abstractmethod $ method "to_json" "self" [] $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "Data ready to serialise as JSON.") [] <> [
      "raise" <+> "NotImplementedError"
    ])

-- -----------------------------------------------------------------------------
-- $ Constructors
--
-- Encode all other cases as a Python dataclass.

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
          googleDocstring mDoc (fmap fieldDocstring fieldDefs) Nothing [] <>
          discriminator name <>
          fields fieldDefs <>
          serde name fieldDefs
        )
      ]


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
      googleDocstring (Just (Docs $ "Return the JSON schema for " <> n <> " data.")) [] (Just "A Python dictionary describing the JSON schema.") [] <> [
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


-- | Generate the JSON serialisation method for a dataclass.
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
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "Data ready to serialise as JSON.") [] <> [
      text "return " <> dict (
        (discriminatorProperty, "self.ADT_TYPE") : fmap serialiseField properties
      )
    ])


-- | Generate the JSON parsing method for a dataclass.
generateFromJson :: Name -> [(Name, Type)] -> Doc a
generateFromJson (Name klass) fieldDefs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fieldDefs
    -- (fieldName, jsonProperty, type)
    properties = fmap (\(n, t) -> (M.findWithDefault n n mangle, n, t)) fieldDefs
    parseField (Name f, Name p, ty@(MaybeT _)) =
      text f <> "=" <> parseType ("data.get(" <> WL.dquotes (text p) <> ", None)") ty <> ","
    parseField (Name f, Name p, ty) =
      text f <> "=" <> parseType ("data[" <> WL.dquotes (text p) <> "]") ty <> ","
  in
    classmethod . method "from_json" "cls" [(Name "data", Right "dict")] $ WL.vsep (
      googleDocstring
        (Just . Docs $ "Validate and parse JSON data into an instance of " <> klass <> ".")
        [(Name "data", Variable (Name "dict"), "JSON data to validate and parse.")]
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
          WL.group (
            "logging.debug(" WL.<##>
              WL.indent 4 (WL.dquotes ("Invalid JSON data received while parsing " <> text klass) <> "," WL.<##> "exc_info=ex") WL.<##>
            ")"
          ),
          "raise"
        ]
      ]
    )

-- -----------------------------------------------------------------------------

-- | Generate the Python expression for a type's JSON schema.
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
    ListT ty' -> dict [
        ("type", WL.dquotes "array"),
        ("item", WL.group (schemaType ty'))
      ]
    MaybeT ty' -> dict [("oneOf", list [
        WL.group (dict [("type", WL.dquotes "null")]),
        WL.group (schemaType ty')
      ])]


-- | Generate the Python expression to serialise a type to JSON.
serialiseType :: Doc a -> Type -> Doc a
serialiseType value ty = case ty of
  Variable _ -> value <> ".to_json()"
  GroundT gr -> case gr of
    StringT -> "str" <> WL.parens value
    BoolT -> value
    IntT -> "int" <> WL.parens value
    LongT -> value
    DoubleT -> value
    UUIDT -> "str" <> WL.parens value
    DateT -> value <> ".isoformat()"
    DateTimeT -> value <> ".strfmt('%Y-%m-%dT%H:%M:%S.%f%z')" -- TODO: We should force timezones?
  ListT ty' -> "[" <> serialiseType "v" ty' <> " for v in " <> value <> "]"
  MaybeT ty' -> "(" <> "lambda v: v and " <> serialiseType "v" ty' <> ")(" <> value <> ")"


-- | Generate the Python expression to parse a type from JSON.
parseType :: Doc a -> Type -> Doc a
parseType value ty = case ty of
  Variable (Name t) -> text t <> ".from_json(" <> value <> ")"
  GroundT gr -> case gr of
    -- TODO: We validate before parsing, so at least some of these probably aren't necessary.
    StringT -> "str" <> WL.parens value
    BoolT -> "bool" <> WL.parens value
    IntT -> "int" <> WL.parens value
    LongT -> "int" <> WL.parens value
    DoubleT -> "float" <> WL.parens value
    UUIDT -> "uuid.UUID" <> WL.parens ("hex=" <> value)
    DateT -> "datetime.date.fromisoformat" <> WL.parens value
    DateTimeT -> "datetime.datetime.strptime("<> value <> ", '%Y-%m-%dT%H:%M:%S.%f%z')"
  ListT ty' -> "[" <> parseType "v" ty' <> " for v in " <> value <> "]"
  MaybeT ty' -> WL.group (
      "(" WL.<##>
        flatIndent 4 ("lambda v: v and " <> parseType "v" ty') WL.<##>
      ")(" WL.<##>
        flatIndent 4 value WL.<##>
      ")"
    )

-- -----------------------------------------------------------------------------

-- | Wrap a Python method definition in a classmethod decorator.
abstractmethod :: Doc a -> Doc a
abstractmethod body =
  WL.vsep [
    string "@abc.abstractmethod", body
  ]

-- | Wrap a Python method definition in a classmethod decorator.
classmethod :: Doc a -> Doc a
classmethod body =
  WL.vsep [
    string "@classmethod", body
  ]

-- | Generate a Python method definition.
method :: Text -> Text -> [(Name, Either Type Text)] -> Doc a -> Doc a
method name self args body =
    WL.vsep [
      string "def" WL.<+> text name <> WL.encloseSep WL.lparen WL.rparen WL.comma (text self : fmap arg args) WL.<> WL.char ':'
    , WL.indent 4 body
    ]
  where
    arg :: (Name, Either Type Text) -> Doc a
    arg (Name n, t) = text n WL.<> WL.char ':' WL.<+> either genTypeV1 text t


-- | Literal Python dictionary.
dict :: [(Text, Doc a)] -> Doc a
dict fs =
     "{" WL.<##> WL.vsep fields' WL.<##> "}"
  where
    prettyKVPair (k,v) = flatIndent 4 $ WL.dquotes (text k) <> ":" <+> v
    fields' = WL.punctuate "," $ fmap prettyKVPair fs


flatIndent :: Int -> Doc a -> Doc a
flatIndent k x = WL.flatAlt (WL.indent k x) (WL.flatten x)


-- | Literal Python list.
list :: [Doc a] -> Doc a
list [] = "[]"
list fs =
    "[" WL.<#> WL.indent 4 (WL.vsep (fmap field fs)) WL.<#> "]"
  where
    field f = f <> ","

string :: [Char] -> Doc a
string =
  WL.text

text :: Text -> Doc a
text =
  WL.text . T.unpack

renderText :: Doc a -> Text
renderText =
  TL.toStrict . WL.displayT . WL.renderPretty 0.8 100
