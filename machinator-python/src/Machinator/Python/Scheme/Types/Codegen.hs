{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Machinator.Python.Scheme.Types.Codegen
(
    genTypesV1
  , genTypeV1
  , genImportsV1
  )
where


import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map (Map)
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
genTypesV1 :: Map Name DataType -> Definition -> Text
genTypesV1 environment = renderText . genTypesV1' environment


-- | Generates a type declaration for the given definition.
genTypesV1' :: Map Name DataType -> Definition -> Doc a
genTypesV1' environment (Definition name mDoc dec) =
  case dec of
    Variant (c1 :| cts)
      | isEnum (c1:cts) -> enum name mDoc (fmap (\(n, d, _) -> (n, d)) $ c1:cts)
      | otherwise       -> WL.vsep $
                               variantclass environment name mDoc (c1:cts)
                             : fmap (\(m, md, fs) -> dataclass environment m (Just name) md fs) (c1:cts)

    Record fts          -> dataclass environment name Nothing mDoc fts

    Newtype _           -> mempty


genTypeV1 :: Map Name DataType -> Type -> Doc a
genTypeV1 environment ty =
  case ty of
    Variable nm
      | Just (Newtype (_, ty')) <- M.lookup nm environment
      -> genTypeV1 environment ty'

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
      text "typing.List" WL.<> WL.brackets (genTypeV1 environment t2)
    MaybeT t2 ->
      string "typing.Optional" <> WL.brackets (genTypeV1 environment t2)
    MapT k v ->
      text "typing.Dict" <> (WL.brackets . WL.hcat) (WL.punctuate ", " [genTypeV1 environment k, genTypeV1 environment v])

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

    arg (nm, ty, comment) = text (unName (M.findWithDefault nm nm mangle)) <> " (" <> genTypeV1 M.empty ty <> "): " <> text comment
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
discriminator
  :: Name       -- ^ Name of the type
  -> Maybe Name -- ^ Name of the supertype
  -> [Doc a]
discriminator _ Nothing = []
discriminator (Name klass) (Just (Name super)) =
    [
      "",
      WL.vsep
        [ "ADT_TYPE" <> ":" <+> "typing.ClassVar" <> WL.brackets "str" <+> "=" <+> (WL.dquotes . text) name
        , "adt_type" <> ":" <+> "str" <+> "=" <+> "dataclasses.field(init=False, repr=False, default=ADT_TYPE)"
        ]
    ]
  where
    name = T.toLower $ if super `T.isSuffixOf` klass then T.dropEnd (T.length super) klass else klass


-- | Generates Python dataclass definitions.
--
-- Note that names are mangled according to Python lexical rules.
fields :: Map Name DataType -> [(Name, Type)] -> [Doc a]
fields _ [] = []
fields environment fs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fs
    field (nm, ty) = case M.findWithDefault nm nm mangle of
      Name n -> Just (text n WL.<> WL.char ':' WL.<+> genTypeV1 environment ty)
  in mapMaybe field fs

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
enum :: Name -> Maybe Docs -> [(Name, Maybe Docs)] -> Doc a
enum n@(Name klass) mDoc ctors =
  let
    instances = fmap (first instanceName) ctors
  in
    WL.linebreak WL.<#>
    WL.vsep [
        string "class" WL.<+> text klass WL.<> WL.parens (string "enum.Enum") WL.<> ":",
        WL.indent 4 . WL.vsep $ (
          googleDocstring mDoc [] Nothing [] <>
          fmap def instances <>
          serdeEnum n (fmap fst instances)
        )
    ]
  where
    instanceName (Name n) = Name (if klass `T.isSuffixOf` n then T.dropEnd (T.length klass) n else n)
    def (Name m, doc) =
      let d = WL.hsep [text  m, WL.char '=', WL.dquotes $ (text . T.toLower) m]
      in case doc of
        Nothing -> d
        docs -> WL.vsep (d : googleDocstring docs [] Nothing [])


-- | Generate the JSON de-/serialisation methods for an enumeration.
serdeEnum :: Name -> [Name] -> [Doc a]
serdeEnum n ctors =
  [ WL.mempty
  , generateJsonSchemaEnum n ctors
  , WL.mempty
  , generateFromJsonEnum n
  , WL.mempty
  , generateToJsonEnum
  , WL.mempty
  , generateJsonKeyEnum n
  ]


-- | Generate the JSON schema method for an enumeration.
generateJsonSchemaEnum :: Name -> [Name] -> Doc a
generateJsonSchemaEnum (Name klass) ctors =
  classmethod . method "json_schema" "cls" [] (Variable (Name "dict")) $ WL.vsep (
    googleDocstring
      (Just . Docs $ "JSON schema for '" <> klass <> "'.")
      []
      (Just "A Python dictionary describing the JSON schema.")
      []
    <> [
      "return " <> dict [
        ("type", WL.dquotes "object"),
        ("properties", dict [
          ("adt_type", dict [
            ("type", WL.dquotes "string"),
            ("enum", list $ fmap (WL.dquotes . text . T.toLower . unName) ctors)
          ])
        ]),
        ("required", list [WL.dquotes (text discriminatorProperty)])
      ]
    ]
  )


-- | Generate the JSON parsing method for an enumeration.
generateFromJsonEnum :: Name -> Doc a
generateFromJsonEnum n@(Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] (Variable n) $ WL.vsep (
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
          text "return" WL.<+> text klass <> WL.parens ("str" <> WL.parens "data['adt_type']")
        ],
        "except jsonschema.exceptions.ValidationError as ex:",
        WL.indent 4 . WL.vsep $ [
          "logging.debug(\"Invalid JSON data received while parsing " <> text klass <> "\", exc_info=ex)",
          "raise"
        ]
      ]
    )


-- | Generated the JSON de-/derialisations methods for enumerations used as a key.
generateJsonKeyEnum :: Name -> Doc a
generateJsonKeyEnum n@(Name klass) =
    WL.vsep [
        fromKey
      , WL.mempty
      , toKey
    ]
  where
    fromKey =
      classmethod . method "from_json_key" "cls" [(Name "data", Right "str")] (Variable n) $ WL.vsep (
        googleDocstring
          (Just . Docs $ "Validate and parse a value from a JSON dictionary key.")
          [(Name "data", GroundT StringT, "JSON data to validate and parse.")]
          (Just $ "An instance of " <> klass <> ".")
          []
        <> [
          "return" <+> text klass <> WL.parens ("str" <> WL.parens "data")
        ]
      )
    toKey =
      method "to_json_key" "self" [] (GroundT StringT) $ WL.vsep (
        googleDocstring
          (Just . Docs $ "Serialised this instanse as a JSON string for use as a dictionary key.")
          []
          (Just $ "A JSON string ready to be used as a key.")
          []
        <> ["return" <+> "str" <> WL.parens "self.value"]
      )


-- | Generate the JSON serialisation method for an enumeration.
generateToJsonEnum :: Doc a
generateToJsonEnum =
  method "to_json" "self" [] (Variable (Name "dict")) $ WL.vsep (
    googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "JSON data ready to be serialised.") [] <> [
    text "return {'adt_type': self.value}"
  ])

-- -----------------------------------------------------------------------------
-- $ Variant classes
--
-- Encode variants as a superclass and a cluster of otherwise normal dataclasses
-- that extend it.


-- | Generate a wrapper class to encode a newtype.
variantclass :: Map Name DataType -> Name -> Maybe Docs -> [(Name, Maybe Docs, [(Name, Type)])] -> Doc a
variantclass environment name@(Name n) mDoc ctors =
  let
    common = variantProperties ctors
  in
    WL.linebreak WL.<#>
    WL.vsep
      [ "@dataclasses.dataclass(frozen=True)"
      , "class" <+> text n <> "(abc.ABC):"
      , WL.indent 4 . WL.vsep $ (
          googleDocstring mDoc (fmap fieldDocstring common) Nothing [] <>
          discriminator (Name "") (Just (Name "")) <>
          fields environment common <>
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
generateJsonSchemaVariant n@(Name klass) =
  classmethod . method "json_schema" "cls" [] (Variable n) $ WL.vsep (
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
          (discriminatorProperty, dict [
            ("type", WL.dquotes "string"),
            ("enum", "adt_types")
          ])
        ]),
        ("required", list [WL.dquotes (text discriminatorProperty)])
      ]
    ]
  )


-- | Generate the JSON parsing method for a variant wrapperclass.
generateFromJsonVariant :: Name -> Doc a
generateFromJsonVariant n@(Name klass) =
  classmethod . method "from_json" "cls" [(Name "data", Right "dict")] (Variable n) $
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
            ),
          "raise" <+> "ValueError(" <> WL.dquotes "Unknown adt_type: '{ty}'" <> ".format(ty=adt_type)" <> ")"
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
  abstractmethod $ method "to_json" "self" [] (Variable (Name "dict")) $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "Data ready to serialise as JSON.") [] <> [
      "raise" <+> "NotImplementedError"
    ])

-- -----------------------------------------------------------------------------
-- $ Constructors
--
-- Encode all other cases as a Python dataclass.

-- | Generates a Python dataclass.
dataclass :: Map Name DataType -> Name -> Maybe Name -> Maybe Docs -> [(Name, Type)] -> Doc a
dataclass environment name@(Name n) super mDoc fieldDefs =
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
          discriminator name super <>
          fields environment fieldDefs <>
          serde environment name (isJust super) fieldDefs
        )
      ]


-- | Generates JSON de-/serialise methods for a Python dataclass.
serde :: Map Name DataType -> Name -> Bool -> [(Name, Type)] -> [Doc a]
serde environment n disc flds =
  [ WL.mempty
  , generateJsonSchema environment n disc flds
  , WL.mempty
  , generateFromJson environment n flds
  , WL.mempty
  , generateToJson environment n disc flds
  ]


-- | Generate the JSON schema method for a Python dataclass.
generateJsonSchema :: Map Name DataType -> Name -> Bool -> [(Name, Type)] -> Doc a
generateJsonSchema environment (Name n) disc flds =
    classmethod . method "json_schema" "cls" [] (Variable (Name "dict")) $ WL.vsep (
      googleDocstring (Just (Docs $ "Return the JSON schema for " <> n <> " data.")) [] (Just "A Python dictionary describing the JSON schema.") [] <> [
      "return" <+> dict [
        ("type", WL.dquotes "object"),
        ("properties", dict (discriminatorField disc <> fmap fieldSchema flds)),
        ("required", list $  discriminatorRequired disc <> (flds >>= requiredField))
      ]
    ])
  where
    discriminatorRequired :: Bool -> [Doc a]
    discriminatorRequired False = []
    discriminatorRequired True = [WL.dquotes (text discriminatorProperty)]
    discriminatorField :: Bool -> [(Text, Doc a)]
    discriminatorField False = []
    discriminatorField True =
      [(discriminatorProperty, dict [("type", WL.dquotes "string"), ("enum", WL.list ["cls.ADT_TYPE"])])]
    fieldSchema :: (Name, Type) -> (Text, Doc a)
    fieldSchema (Name f, ty) = (f, schemaType environment ty)

    requiredField (Name f, ty) =
      case ty of
        MaybeT _ -> []
        _        -> [WL.dquotes (text f)]


-- | Generate the JSON serialisation method for a dataclass.
generateToJson :: Map Name DataType ->  Name -> Bool -> [(Name, Type)] -> Doc a
generateToJson environment _ disc fieldDefs =
  let
    mangle = mangleNames (S.fromList (fmap fst fieldDefs))
    -- (fieldName, jsonProperty, type)
    properties = fmap (\(n, t) -> (M.findWithDefault n n mangle, n, t)) fieldDefs
    serialiseField (Name f, Name p, ty) =
      (p, serialiseType environment ("self." <> text f) ty)
    discriminatorField False = []
    discriminatorField True = [(discriminatorProperty, "self.ADT_TYPE")]
  in
    method "to_json" "self" [] (Variable (Name "dict")) $ WL.vsep (
      googleDocstring (Just (Docs "Serialise this instance as JSON.")) [] (Just "Data ready to serialise as JSON.") [] <> [
      text "return " <> dict (
        discriminatorField disc <> fmap serialiseField properties
      )
    ])


-- | Generate the JSON parsing method for a dataclass.
generateFromJson :: Map Name DataType -> Name -> [(Name, Type)] -> Doc a
generateFromJson environment name@(Name klass) fieldDefs =
  let
    mangle = mangleNames . S.fromList . fmap fst $ fieldDefs
    -- (fieldName, jsonProperty, type)
    properties = fmap (\(n, t) -> (M.findWithDefault n n mangle, n, t)) fieldDefs
    parseField (Name f, Name p, ty@(MaybeT _)) =
      text f <> "=" <> parseType environment ("data.get(" <> WL.dquotes (text p) <> ", None)") ty <> ","
    parseField (Name f, Name p, ty) =
      text f <> "=" <> parseType environment ("data[" <> WL.dquotes (text p) <> "]") ty <> ","
  in
    classmethod . method "from_json" "cls" [(Name "data", Right "dict")] (Variable name) $ WL.vsep (
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
schemaType :: Map Name DataType -> Type -> Doc a
schemaType environment ty =
  case ty of
    Variable nm
      | Just (Newtype (_, ty')) <- M.lookup nm environment
      -> schemaType environment ty'
    Variable (Name v) -> text v <> ".json_schema()"
    GroundT gr -> case gr of
      StringT -> dict [("type", WL.dquotes "string")]
      BoolT -> dict [("type", WL.dquotes "boolean")]
      IntT -> dict [("type", WL.dquotes "integer")]
      LongT -> dict [("type", WL.dquotes "integer")]
      DoubleT -> dict [("type", WL.dquotes "number")]
      UUIDT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "uuid")]
      DateT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "date")]
      DateTimeT -> dict [("type", WL.dquotes "string"), ("format", WL.dquotes "date-time")]
    ListT ty' -> dict [
        ("type", WL.dquotes "array"),
        ("item", WL.group (schemaType environment ty'))
      ]
    MaybeT ty' -> dict [("oneOf", list [
        WL.group (dict [("type", WL.dquotes "null")]),
        WL.group (schemaType environment ty')
      ])]
    MapT _ v -> dict [
        ("type", WL.dquotes "object"),
        ("additionalProperties", schemaType environment v)
      ]

-- | JSON keys are strings, we need to serialise the JSON encoding to a string.
serialiseKey :: Map Name DataType -> Doc a -> Type -> Doc a
serialiseKey environment value ty = case ty of
  Variable nm
    | Just (Newtype (_, ty')) <- M.lookup nm environment
    -> serialiseKey environment value ty'
  Variable _ -> value <> ".to_json_key()"
  GroundT gr -> case gr of
    StringT -> "str" <> WL.parens value
    BoolT -> "str" <> WL.parens value
    IntT -> "str" <> WL.parens value
    LongT -> "str" <> WL.parens value
    DoubleT -> "str" <> WL.parens value
    UUIDT -> "str" <> WL.parens value
    DateT -> value <> ".isoformat()"
    DateTimeT -> value <> ".strftime('%Y-%m-%dT%H:%M:%S.%f%z')" -- TODO: We should force timezones?
  ListT _ -> "raise ValueError('Cannot use lists as keys')"
  MaybeT _ -> "raise ValueError('Cannot use optional values as keys')"
  MapT _ _ -> "raise ValueError('Cannot use maps as keys')"

-- | Generate the Python expression to serialise a type to JSON.
serialiseType :: Map Name DataType -> Doc a -> Type -> Doc a
serialiseType environment value ty = case ty of
  Variable nm
    | Just (Newtype (_, ty')) <- M.lookup nm environment
    -> serialiseType environment value ty'
  Variable _ -> value <> ".to_json()"
  GroundT gr -> case gr of
    StringT -> "str" <> WL.parens value
    BoolT -> value
    IntT -> "int" <> WL.parens value
    LongT -> value
    DoubleT -> value
    UUIDT -> "str" <> WL.parens value
    DateT -> value <> ".isoformat()"
    DateTimeT -> value <> ".strftime('%Y-%m-%dT%H:%M:%S.%f%z')" -- TODO: We should force timezones?
  ListT ty' -> "[" <> serialiseType environment "v" ty' <> " for v in " <> value <> "]"
  MaybeT ty' -> "(" <> "lambda v: v and " <> serialiseType environment "v" ty' <> ")(" <> value <> ")"
  -- TODO: This just blindly assumes that k will be serialised to a str and not any other JSON document.
  MapT k v -> "{" <> serialiseKey environment "k" k <> ":" <+> serialiseType environment "v" v <+> "for" <+> "k, v" <+> "in" <+> value <> ".items()}"


-- | JSON keys are strings, we need to serialise the JSON encoding to a string.
parseKey :: Map Name DataType -> Doc a -> Type -> Doc a
parseKey environment value ty = case ty of
  Variable nm
    | Just (Newtype (_, ty')) <- M.lookup nm environment
    -> parseKey environment value ty'
  Variable (Name t) -> text t <> ".from_json_key" <> WL.parens value
  GroundT gr -> case gr of
    StringT -> "str" <> WL.parens value
    BoolT -> "bool" <> WL.parens value -- TODO
    IntT -> "int" <> WL.parens value
    LongT -> "int" <> WL.parens value
    DoubleT -> "float" <> WL.parens value
    UUIDT -> "uuid.UUID" <> WL.parens ("hex=" <> value)
    DateT -> "datetime.date.fromisoformat" <> WL.parens value
    DateTimeT -> "isodate.parse_datetime("<> value <> ")"
  ListT _ -> "raise ValueError('Cannot use lists as keys')"
  MaybeT _ -> "raise ValueError('Cannot use optional values as keys')"
  MapT _ _ -> "raise ValueError('Cannot use maps as keys')"

-- | Generate the Python expression to parse a type from JSON.
parseType :: Map Name DataType -> Doc a -> Type -> Doc a
parseType environment value ty = case ty of
  Variable nm
    | Just (Newtype (_, ty')) <- M.lookup nm environment
    -> parseType environment value ty'
  Variable (Name t) -> text t <> ".from_json" <> WL.parens value
  GroundT gr -> case gr of
    -- TODO: We validate before parsing, so at least some of these probably aren't necessary.
    StringT -> "str" <> WL.parens value
    BoolT -> "bool" <> WL.parens value
    IntT -> "int" <> WL.parens value
    LongT -> "int" <> WL.parens value
    DoubleT -> "float" <> WL.parens value
    UUIDT -> "uuid.UUID" <> WL.parens ("hex=" <> value)
    DateT -> "datetime.date.fromisoformat" <> WL.parens value
    DateTimeT -> "isodate.parse_datetime("<> value <> ")"
  ListT ty' -> "[" <> parseType environment "v" ty' <> " for v in " <> value <> "]"
  MaybeT ty' -> WL.group (
      "(" WL.<##>
        flatIndent 4 ("lambda v: v and " <> parseType environment "v" ty') WL.<##>
      ")(" WL.<##>
        flatIndent 4 value WL.<##>
      ")"
    )
  MapT k v ->
    WL.group (
      "{" WL.<##>
        flatIndent 4 (parseKey environment "k" k <> ":" <+> parseType environment "v" v <+> "for" <+> "k, v" <+> "in" <+> value <> ".items()") WL.<##>
      "}"
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
method :: Text -> Text -> [(Name, Either Type Text)] -> Type -> Doc a -> Doc a
method name self args ret body =
    WL.vsep [
      string "def" WL.<+> text name <> WL.encloseSep WL.lparen WL.rparen WL.comma (text self : fmap arg args) WL.<+> "->" WL.<+> genTypeV1 M.empty ret WL.<> WL.char ':'
    , WL.indent 4 body
    ]
  where
    arg :: (Name, Either Type Text) -> Doc a
    arg (Name n, t) = text n WL.<> WL.char ':' WL.<+> either (genTypeV1 M.empty) text t


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
