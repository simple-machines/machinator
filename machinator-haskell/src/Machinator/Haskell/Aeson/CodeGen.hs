{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Haskell.Aeson.CodeGen (
    generateAesonModuleV1
  , generateAesonV1
  , generateToJsonNameV1
  , generateToJsonSigV1
  , generateToJsonV1
  , generateFromJsonNameV1
  , generateFromJsonSigV1
  , generateFromJsonV1
  ) where


import qualified Data.Text as T

import           Language.Haskell.TH (Dec, Exp)
import qualified Language.Haskell.TH as TH

import qualified Machinator.Core as M
import qualified Machinator.Core.Data.Definition as M

import           P

import qualified X.Language.Haskell.TH.Syntax as XTH


generateAesonModuleV1 :: [M.Definition] -> Text
generateAesonModuleV1 defs =
  T.unlines [
      "import qualified Control.Monad"
    , "import qualified Data.Aeson"
    , "import qualified Data.Aeson.Types"
    , "import qualified Data.Functor"
    , "import           Data.Text (Text)"
    , T.unlines (fmap (T.pack . TH.pprint) (generateAesonV1 defs))
    ]

generateAesonV1 :: [M.Definition] -> [Dec]
generateAesonV1 defs =
  fold . flip fmap defs $ \def@(M.Definition n _ty) -> [
      generateToJsonSigV1 n
    , XTH.val_ (XTH.varP (generateToJsonNameV1 n)) (generateToJsonV1 def)
    , generateFromJsonSigV1 n
    , XTH.val_ (XTH.varP (generateFromJsonNameV1 n)) (generateFromJsonV1 def)
    ]

generateToJsonNameV1 :: M.Name -> TH.Name
generateToJsonNameV1 (M.Name n) =
  TH.mkName $
    "toJsonV1" <> T.unpack n

generateToJsonSigV1 :: M.Name -> Dec
generateToJsonSigV1 tn@(M.Name n) =
  XTH.sig (generateToJsonNameV1 tn) (XTH.arrowT_ (XTH.conT (XTH.mkName_ n)) (XTH.conT (XTH.mkName_ "Data.Aeson.Value")))

generateToJsonV1 :: M.Definition -> Exp
generateToJsonV1 (M.Definition (M.Name tn) typ) =
  XTH.lamE [XTH.varP (TH.mkName "x")] $
    case typ of
      M.Variant cts ->
        XTH.caseE (XTH.varE (TH.mkName "x")) $
          flip fmap (toList cts) $ \(M.Name n, fts) ->
            let
              pats = fmap (T.unpack . M.unName . fst) fts
            in
              XTH.match_
                (XTH.conP (XTH.mkName_ n) (fmap (XTH.varP . TH.mkName) pats))
                (object $
                    field "adt_type" (TH.SigE (XTH.litE (XTH.stringL_ n)) text_)
                  : toJsonFields (fmap (first (T.unpack . M.unName)) fts)
                )
      M.Record fts ->
        XTH.caseE (XTH.varE (TH.mkName "x")) . (:[]) $
          let
            pats = fmap (T.unpack . M.unName . fst) fts
          in
            XTH.match_
              (XTH.conP (XTH.mkName_ tn) (fmap (XTH.varP . TH.mkName) pats))
              (object $ toJsonFields (fmap (first (T.unpack . M.unName)) fts))

      M.Newtype (M.Name fieldName, mty) ->
        XTH.caseE (XTH.varE (TH.mkName "x")) . (:[]) $
          let
            fn   = T.unpack fieldName
          in
            XTH.match_
              (XTH.conP (XTH.mkName_ tn) (fmap (XTH.varP . TH.mkName) [fn]))
              (XTH.appE (typeToJson mty) (XTH.varE (TH.mkName fn)))


toJsonFields :: [([Char], M.Type)] -> [Exp]
toJsonFields fts =
  flip fmap fts $ \(n, mty) ->
    field (T.pack n) (XTH.appE (typeToJson mty) (XTH.varE (TH.mkName n)))

typeToJson :: M.Type -> Exp
typeToJson ty =
  case ty of
    M.Variable n ->
      XTH.varE (generateToJsonNameV1 n)
    M.ListT t2 ->
      fmap_ (typeToJson t2)
    M.MaybeT t2 ->
      fmap_ (typeToJson t2)
    M.GroundT g ->
      case g of
        M.StringT ->
          toJson_
        M.BoolT ->
          toJson_
        M.IntT ->
          toJson_
        M.LongT ->
          toJson_
        M.DoubleT ->
          toJson_
        M.UUIDT ->
          toJson_
        M.DateT ->
          toJson_
        M.DateTimeT ->
          toJson_

generateFromJsonNameV1 :: M.Name -> TH.Name
generateFromJsonNameV1 (M.Name n) =
  TH.mkName $
    "fromJsonV1" <> T.unpack n


generateFromJsonSigV1 :: M.Name -> Dec
generateFromJsonSigV1 tn@(M.Name n) =
  XTH.sig
    (generateFromJsonNameV1 tn)
    (XTH.arrowT_
      (XTH.conT (XTH.mkName_ "Data.Aeson.Value"))
      (XTH.appT (XTH.conT (XTH.mkName_ "Data.Aeson.Types.Parser")) (XTH.conT (XTH.mkName_ n))))

generateFromJsonV1 :: M.Definition -> Exp
generateFromJsonV1 (M.Definition tn@(M.Name n) typ) =
  XTH.lamE [XTH.varP (TH.mkName "x")] $
    case typ of
      M.Variant cts ->
        withObject__ (T.unpack n) (XTH.varE (TH.mkName "x")) $
          XTH.lamE [XTH.varP (TH.mkName "o")] $
            TH.DoE [
                TH.BindS (XTH.varP (TH.mkName "adt_type")) (XTH.varE (TH.mkName "o") .: "adt_type")
              , TH.NoBindS $
                  XTH.caseE (TH.SigE (XTH.varE (TH.mkName "adt_type")) text_) . orFail n $
                    let
                      matchTag s = XTH.match_ (TH.LitP (XTH.stringL_ s))
                    in
                      flip fmap (toList cts) $ \(cn@(M.Name cnn), fts) ->
                        matchTag cnn (fromJsonFields cn (fmap (first (T.unpack . M.unName)) fts))
              ]
      M.Record fts ->
        withObject__ (T.unpack n) (XTH.varE (TH.mkName "x")) $
          XTH.lamE [XTH.varP (TH.mkName "o")] $
            fromJsonFields tn (fmap (first (T.unpack . M.unName)) fts)
      M.Newtype (M.Name fieldName, _) ->
        let fieldNameX = TH.mkName . T.unpack $ fieldName
          in TH.DoE [
              TH.BindS (XTH.varP fieldNameX) (XTH.appE parseJson_ (XTH.varE (TH.mkName "x")))
            , TH.NoBindS (return_ (XTH.applyE (XTH.conE (XTH.mkName_ n)) [XTH.varE fieldNameX]))
            ]


orFail :: Text -> [TH.Match] -> [TH.Match]
orFail n ms =
  ms <> [XTH.match_ (TH.WildP) (fail_ n)]

fromJsonFields :: M.Name -> [([Char], M.Type)] -> Exp
fromJsonFields (M.Name n) fts =
  TH.DoE $
        (flip fmap fts $ \(fn, ft) ->
          TH.BindS (XTH.varP (TH.mkName fn)) (parseFieldWith (XTH.varE (TH.mkName "o")) (T.pack fn) (typeFromJson ft)))
    <> [TH.NoBindS (return_ (XTH.applyE (XTH.conE (XTH.mkName_ n)) (fmap (XTH.varE . TH.mkName . fst) fts)))]

typeFromJson :: M.Type -> Exp
typeFromJson ty =
  case ty of
    M.Variable n ->
      XTH.varE (generateFromJsonNameV1 n)
    M.ListT t2 ->
      mapM__ (typeFromJson t2)
    M.MaybeT t2 ->
      mapM__ (typeFromJson t2)
    M.GroundT g ->
      case g of
        M.StringT ->
          parseJson_
        M.BoolT ->
          parseJson_
        M.IntT ->
          parseJson_
        M.LongT ->
          parseJson_
        M.DoubleT ->
          parseJson_
        M.UUIDT ->
          parseJson_
        M.DateT ->
          parseJson_
        M.DateTimeT ->
          parseJson_

-- -----------------------------------------------------------------------------

object :: [Exp] -> Exp
object es =
  XTH.appE
    (XTH.varE (TH.mkName "Data.Aeson.object"))
    (XTH.listE es)

field :: Text -> Exp -> Exp
field fn v =
  TH.InfixE
    (Just (XTH.litE (XTH.stringL_ fn)))
    (XTH.varE (TH.mkName "Data.Aeson..="))
    (Just v)

mapM__ :: Exp -> Exp
mapM__ =
  XTH.appE
    (XTH.varE (TH.mkName "Control.Monad.mapM"))

fmap_ :: Exp -> Exp
fmap_ f =
  (XTH.appE
    (XTH.varE (TH.mkName "Data.Functor.fmap"))
    f)

return_ :: Exp -> Exp
return_ =
  XTH.appE (XTH.varE (TH.mkName "return"))

toJson_ :: Exp
toJson_ =
  XTH.varE (TH.mkName "Data.Aeson.toJSON")

parseJson_ :: Exp
parseJson_ =
  XTH.varE (TH.mkName "Data.Aeson.parseJSON")

parseFieldWith :: Exp -> Text -> Exp -> Exp
parseFieldWith e f p =
  TH.InfixE
    (Just (e .: f))
    (XTH.varE (TH.mkName "Control.Monad.>>="))
    (Just p)

(.:) :: Exp -> Text -> Exp
e .: f =
  TH.InfixE
    (Just e)
    (XTH.varE (TH.mkName "Data.Aeson..:"))
    (Just (XTH.litE (XTH.stringL_ f)))

withObject_ :: Exp
withObject_ =
  XTH.varE (TH.mkName "Data.Aeson.Types.withObject")

withObject__ :: [Char] -> Exp -> Exp -> Exp
withObject__ msg v p =
  XTH.applyE withObject_ [
      XTH.litE (XTH.stringL msg)
    , p
    , v
    ]

text_ :: TH.Type
text_ =
  (XTH.conT (TH.mkName "Text"))

fail_ :: Text -> Exp
fail_ t =
  XTH.appE (XTH.varE (TH.mkName "Control.Monad.fail")) (XTH.litE (XTH.stringL_ t))
