{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Scheme.Circe.Codegen (
    generateCirceV1
  , generateToJsonV1Companion
  , generateToJsonV1
  , generateFromJsonV1
  ) where


import qualified Data.List as L
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
  concat . with defs $ \def@(M.Definition n _ty) -> [
      generateToJsonV1 def
    , generateFromJsonV1 def
    ]


generateToJsonV1Companion :: M.Definition -> Text
generateToJsonV1Companion def@(M.Definition (M.Name tn) _) =
  renderText $
    text "object" <+> text tn <+> text "{" <> WL.hardline <>
      WL.indent 2 (
        WL.vsep [
          generateToJsonV1 def
        , generateFromJsonV1 def
        ]
      )  <> WL.hardline <> text "}"



generateToJsonV1 :: M.Definition -> Doc a
generateToJsonV1 (M.Definition (M.Name tn) typ) =
  text "implicit val" <+>
    text tn <> text "Encoder" <> text ":" <+> text "io.circe.Encoder" <> WL.brackets (text tn) <+> text "= {" <> WL.hardline <>
      WL.vsep [
        WL.indent 2 $
          case typ of
            M.Variant cts ->
              WL.vsep $
                with (toList cts) $ \(M.Name n, fts) ->
                  text "case" <+> (
                    WL.hang 2 $ text n <> WL.tupled (with fts $ \(M.Name n, _) -> text n) <+> text "=>" <> WL.softline <>
                      object (
                        field "adt_type" (text "io.circe.Json.fromString" <> WL.parens (WL.dquotes (makeDiscriminator tn n)))
                      : (with fts $ \(M.Name n, typ) -> field n (text "io.circe.Encoder" <> WL.brackets (genTypeV1 typ) <> text ".apply" <> WL.parens (text n)))
                      )
                  )
            M.Record fts ->
              text "case" <+> (
                WL.hang 2 $ text tn <> WL.tupled (with fts $ \(M.Name n, _) -> text n) <+> text "=>" <> WL.softline <>
                  object (
                    with fts $ \(M.Name n, typ) -> field n (text "io.circe.Encoder" <> WL.brackets (genTypeV1 typ) <> text ".apply" <> WL.parens (text n))
                  )
              )
      , text "}"
      ]


generateFromJsonV1 :: M.Definition -> Doc a
generateFromJsonV1 def@(M.Definition (M.Name tn) typ) =
  text "implicit val" <+>
    text tn <> text "Decoder" <> text ":" <+> text "io.circe.Decoder" <> WL.brackets (text tn) <+> text "=" <> WL.hardline <>
      WL.indent 2 (
        text "(c: io.circe.HCursor)" <+> "=>" <> WL.hardline <>
          WL.indent 2 (
            case typ of
              M.Variant cts ->
                text "c.downField(\"adt_type\").as[String] flatMap {" <> WL.hardline <> (
                  WL.vsep [
                    WL.indent 2 $
                      WL.vsep $
                        (with (toList cts) $ \(M.Name n, fts) ->
                          text "case" <+> WL.dquotes (makeDiscriminator tn n) <+> text "=>" <> WL.hardline <>
                            (WL.indent 2 $
                              case fts of
                                [] ->
                                  text "Right" <> WL.parens (text n <> "()")
                                _ ->
                                  text "for" <+> text "{" <> WL.hardline <>
                                    WL.indent 2 (
                                      WL.vsep $
                                        with fts $ \(M.Name f, ft) ->
                                          text f <+> text "<-" <+> text "c.downField(\"" <> text f <>"\").as[" <> genTypeV1 ft <> "]"
                                    ) <> WL.hardline <>
                                  text "}" <+> text "yield" <+> text n <> WL.tupled (with fts $ \(M.Name n, ty) -> text n)
                            ))
                          <> [
                            text "case unknown =>" <> WL.hardline <>
                              (WL.indent 2 $
                                text "Left(io.circe.DecodingFailure(s\"Unknown ADT constructor $unknown.\", c.history))"
                              )
                          ]
                    ] <> WL.hardline <> text "}"
                )

              M.Record fts ->
                WL.indent 2 (
                  text "for" <+> text "{" <> WL.hardline <>
                    WL.indent 2 (
                      WL.vsep $
                        with fts $ \(M.Name f, ft) ->
                          text f <+> text "<-" <+> text "c.downField(\"" <> text f <>"\").as[" <> genTypeV1 ft <> "]"
                    ) <> WL.hardline <>
                  text "}" <+> text "yield" <+> text tn <> WL.tupled (with fts $ \(M.Name n, ty) -> text n)
                )
          )
      )

-- -----------------------------------------------------------------------------

object :: [Doc a] -> Doc a
object es =
  text "io.circe.Json.obj" <>
    WL.tupled es


field :: Text -> Doc a -> Doc a
field fn v =
  WL.dquotes (text fn) <+> ("->") <+> v


toJson_ :: Doc a
toJson_ =
  text "Encoder[A].apply"


string :: [Char] -> Doc a
string =
  WL.text

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
