{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Machinator.Scala.Scheme.Circe.Codegen (
    generateCirceV1
  , generateToJsonV1Companion
  , generateToJsonV1
  , generateFromJsonV1
  ) where


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import qualified Machinator.Core as M
import qualified Machinator.Core.Data.Definition as M

import           Machinator.Scala.Scheme.Types.Codegen (genTypeV1)

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL


generateCirceV1 :: [M.Definition] -> [Doc a]
generateCirceV1 defs =
  concat . with defs $ \def -> [
      generateToJsonV1 def
    , generateFromJsonV1 def
    ] <> generateCirceKeyCodecsV1 def


generateToJsonV1Companion :: M.Definition -> Text
generateToJsonV1Companion def@(M.Definition (M.Name tn) _ _) =
  renderText $
    text "object" <+> text tn <+>
      code_block (
        [
          generateToJsonV1 def
        , generateFromJsonV1 def
        ] <> generateCirceKeyCodecsV1 def
      )

typeNameV1 :: M.Definition -> Doc a
typeNameV1 (M.Definition (M.Name tn) _ (M.Variant _)) = text tn
typeNameV1 (M.Definition (M.Name tn) _ (M.Record _)) = text tn
typeNameV1 (M.Definition (M.Name tn) _ (M.Newtype _)) = text tn

generateToJsonV1 :: M.Definition -> Doc a
generateToJsonV1 def@(M.Definition (M.Name tn) _ typ) =
  text "implicit val"
    <+> text tn <> text "Encoder" <> text ":"
    <+> text "io.circe.Encoder" <> WL.brackets (typeNameV1 def)
    <+> text "="
    <+> code_block
      [ case typ of
          M.Variant cts ->
            WL.vsep $
              with (toList cts) $ \(M.Name n, _, fts) ->
                case fts of
                  [] ->
                    case_expr
                      (text n)
                      ( object
                          [ field "adt_type" (text "io.circe.Json.fromString" <> WL.parens (WL.dquotes (makeDiscriminator tn n)))
                          ]
                      )
                  _ ->
                    case_expr
                      (text n <> WL.tupled (with fts $ \(M.Name fn, _) -> text fn))
                      ( object $
                          field "adt_type" (text "io.circe.Json.fromString" <> WL.parens (WL.dquotes (makeDiscriminator tn n))) :
                          with fts (\(M.Name fn, f'typ) -> field fn (text "io.circe.Encoder" <> WL.brackets (genTypeV1 f'typ) <> text ".apply" <> WL.parens (text fn)))
                      )
          M.Record fts ->
            case_expr
              (text tn <> WL.tupled (with fts $ \(M.Name n, _) -> text n))
              ( object $
                  with fts $ \(M.Name n, f'typ) -> field n (text "io.circe.Encoder" <> WL.brackets (genTypeV1 f'typ) <> text ".apply" <> WL.parens (text n))
              )
          M.Newtype (M.Name wrapper, wrappedType) ->
            case_expr
              (text tn <> WL.tupled [text wrapper])
              (text "io.circe.Encoder" <> WL.brackets (genTypeV1 wrappedType) <> text ".apply" <> WL.parens (text wrapper))
      ]

generateFromJsonV1 :: M.Definition -> Doc a
generateFromJsonV1 def@(M.Definition (M.Name tn) _ typ) =
  text "implicit val"
    <+> text tn
    <> text "Decoder"
    <> text ":"
    <+> text "io.circe.Decoder"
    <> WL.brackets (typeNameV1 def)
    <+> text "="
    <> WL.hardline
    <> WL.indent 2
      ( text "(c: io.circe.HCursor)" <+> "=>" <> WL.hardline
          <> WL.indent 2
            ( case typ of
                M.Variant cts ->
                  text "c.downField(\"adt_type\").as[String] flatMap"
                    <+> code_block
                      ( with
                          (toList cts)
                          ( \(M.Name n, _, fts) ->
                              case_expr
                                (WL.dquotes (makeDiscriminator tn n))
                                ( case fts of
                                    [] ->
                                      text "Right" <> WL.parens (text n)
                                    _ ->
                                      for_yield
                                        ( with fts $ \case
                                            (M.Name f, ft@(M.ListT _)) ->
                                              (text f, text "c.downField(\"" <> text f <> "\").as[Option[" <> genTypeV1 ft <> "]].map(_.getOrElse(Nil))")
                                            (M.Name f, ft@(M.MapT _ _)) ->
                                              (text f, text "c.downField(\"" <> text f <> "\").as[Option[" <> genTypeV1 ft <> "]].map(_.getOrElse(Map.empty))")
                                            (M.Name f, ft) ->
                                              (text f, text "c.downField(\"" <> text f <> "\").as[" <> genTypeV1 ft <> "]")
                                        )
                                        (text n <> WL.tupled (with fts $ \(M.Name fn, _) -> text fn))
                                )
                          )
                          <> [ case_expr
                                 (text "unknown")
                                 (text "Left(io.circe.DecodingFailure(s\"Unknown ADT constructor $unknown.\", c.history))")
                             ]
                      )
                M.Record [] ->
                  text "Right" <> WL.parens (text tn <> WL.tupled [])
                M.Record fts ->
                  for_yield
                    ( with fts $ \case
                        (M.Name f, ft@(M.ListT _)) ->
                          (text f, text "c.downField(\"" <> text f <> "\").as[Option[" <> genTypeV1 ft <> "]].map(_.getOrElse(Nil))")
                        (M.Name f, ft@(M.MapT _ _)) ->
                          (text f, text "c.downField(\"" <> text f <> "\").as[Option[" <> genTypeV1 ft <> "]].map(_.getOrElse(Map.empty))")
                        (M.Name f, ft) ->
                          (text f, text "c.downField(\"" <> text f <> "\").as[" <> genTypeV1 ft <> "]")
                    )
                    (text tn <> WL.tupled (with fts $ \(M.Name n, _) -> text n))
                M.Newtype (M.Name wrapper, wrappedType) ->
                  for_yield
                    [(text wrapper, text "c.as[" <> genTypeV1 wrappedType <> "]")]
                    (text tn <> WL.tupled [text wrapper])
            )
      )

generateCirceKeyCodecsV1 :: M.Definition -> [Doc a]
generateCirceKeyCodecsV1 def@(M.Definition (M.Name tn) _ (M.Newtype (M.Name wrapper, wrappedType@(M.GroundT _)))) =
  [
    text "implicit val"
      <+> text tn <> text "KeyEncoder" <> text ":"
      <+> text "io.circe.KeyEncoder" <> WL.brackets (typeNameV1 def)
      <+> text "="
      <+> code_block
        [ case_expr
            (text tn <> WL.tupled [text wrapper])
            (text "io.circe.KeyEncoder" <> WL.brackets (genTypeV1 wrappedType) <> text ".apply" <> WL.parens (text wrapper))
        ],
    text "implicit val"
      <+> text tn <> text "KeyDecoder" <> text ":"
      <+> text "io.circe.KeyDecoder" <> WL.brackets (typeNameV1 def)
      <+> text "="
      <+> code_block
        [
          text "io.circe.KeyDecoder" <> WL.brackets (genTypeV1 wrappedType) <> text ".map" <> WL.parens (text tn <> ".apply")
        ]
  ]
generateCirceKeyCodecsV1 _ = []

-- -----------------------------------------------------------------------------

for_yield :: [(Doc a, Doc a)] -> Doc a -> Doc a
for_yield binds yield =
  let
    bindings =
      with binds $ \(binding, expr) ->
        binding <+> text "<-" <+> expr
  in
    text "for" <+> code_block bindings <+> text "yield" <+> yield


code_block :: [Doc a] -> Doc a
code_block exprs =
  WL.vsep [
    text "{"
  , WL.indent 2 $
      WL.vsep exprs
  , text "}"
  ]

case_expr :: Doc a -> Doc a -> Doc a
case_expr discriminator rhs =
  WL.vsep [
    text "case" <+> discriminator <+> text "=>"
  , WL.indent 2 rhs
  ]


object :: [Doc a] -> Doc a
object es =
  text "io.circe.Json.obj" <>
    WL.tupled es


field :: Text -> Doc a -> Doc a
field fn v =
  WL.dquotes (text fn) <+> "->" <+> v

text :: Text -> Doc a
text =
  WL.text . T.unpack


renderText :: Doc a -> Text
renderText =
  TL.toStrict . WL.displayT . WL.renderPrettyDefault

makeDiscriminator :: Text -> Text -> Doc a
makeDiscriminator parent constructor =
  text $
    T.toLower $
      fromMaybe constructor $
        T.stripSuffix parent constructor
