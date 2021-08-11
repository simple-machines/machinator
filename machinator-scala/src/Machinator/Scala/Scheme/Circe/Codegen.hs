{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Scheme.Circe.Codegen (
    generateCirceModuleV1
  , generateCirceV1
  , generateToJsonV1
  , generateFromJsonNameV1
  , generateFromJsonV1
  ) where


import qualified Data.List as L
import qualified Data.Text as T

import qualified Machinator.Core as M
import qualified Machinator.Core.Data.Definition as M

import           Machinator.Scala.Scheme.Types.Codegen (genTypeV1)

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL

generateCirceModuleV1 :: [M.Definition] -> Doc a
generateCirceModuleV1 defs =
  WL.vsep $ concat [
      [text "import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}"]
    , generateCirceV1 defs
  ]


generateCirceV1 :: [M.Definition] -> [Doc a]
generateCirceV1 defs =
  concat . with defs $ \def@(M.Definition n _ty) -> [
      generateToJsonV1 def
    , generateFromJsonV1 def
    ]

-- generateToJsonNameV1 :: M.Name -> Doc a
-- generateToJsonNameV1 (M.Name n) =
--   TH.mkName $
--     "toJsonV1" <> T.unpack n

-- generateToJsonSigV1 :: M.Name -> Doc a
-- generateToJsonSigV1 tn@(M.Name n) =
--   XTH.sig (generateToJsonNameV1 tn) (XTH.arrowT_ (XTH.conT (XTH.mkName_ n)) (XTH.conT (XTH.mkName_ "Data.Circe.Value")))

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
                        field "adt_type" (text "io.circe.Json.fromString" <> WL.parens (WL.dquotes (text n)))
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


toJsonFields :: [([Text], M.Type)] -> Doc a
toJsonFields fts =
  mempty

typeToJson :: M.Type -> Doc a
typeToJson ty =
  mempty

generateFromJsonNameV1 :: M.Name -> Doc a
generateFromJsonNameV1 (M.Name n) =
  mempty


generateFromJsonSigV1 :: M.Name -> Doc a
generateFromJsonSigV1 tn@(M.Name n) =
  mempty

generateFromJsonV1 :: M.Definition -> Doc a
generateFromJsonV1 def@(M.Definition (M.Name n) _typ) =
  mempty

matchTagFields :: M.Definition -> Doc a
matchTagFields (M.Definition tn@(M.Name n) typ) =
  mempty

orFail :: Text -> [Doc a] -> [Doc a]
orFail n ms =
  mempty

fromJsonFields :: M.Name -> [([Char], M.Type)] -> Doc a
fromJsonFields (M.Name n) fts =
  mempty

typeFromJson :: M.Type -> Doc a
typeFromJson ty =
  mempty

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
