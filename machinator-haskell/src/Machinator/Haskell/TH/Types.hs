{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Haskell.TH.Types (
  -- * Interface
    genTypesV1
  -- * Guts and utilities
  , genConV1
  , genRecV1
  , genRecFieldNameV1
  , genTypeV1
  ) where


import qualified Data.Char as Char
import qualified Data.Text as T

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import qualified Language.Haskell.TH as TH
import qualified X.Language.Haskell.TH.Syntax as XTH


-- | Generate a TH type declaration from a Machinator 'Definition'.
genTypesV1 :: Definition -> TH.Dec
genTypesV1 (Definition nn@(Name n) d) =
  case d of
    Variant nts ->
      XTH.data_ (XTH.mkName_ n) [] (fmap (uncurry genConV1) (toList nts))
    Record fts ->
      XTH.data_ (XTH.mkName_ n) [] [genRecV1 nn fts]
    Newtype fts ->
      TH.NewtypeD [] (XTH.mkName_ n) [] Nothing (genNtV1 nn fts) []

-- | Generate a regular variant constructor.
genConV1 :: Name -> [(Name, Type)] -> TH.Con
genConV1 (Name n) ts =
  XTH.normalC_' (XTH.mkName_ n) (fmap (genTypeV1 . snd) ts)

-- | Generate a record constructor.
genRecV1 :: Name -> [(Name, Type)] -> TH.Con
genRecV1 nn@(Name n) fts =
  XTH.recC_' (XTH.mkName_ n) (fmap (bimap (genRecFieldNameV1 nn) genTypeV1) fts)

-- | Generate a record constructor.
genNtV1 :: Name -> (Name, Type) -> TH.Con
genNtV1 nn@(Name n) fts =
  XTH.recC_ (XTH.mkName_ n) [bimap (genRecFieldNameV1 nn) genTypeV1 fts]

-- | The heuristic used to derive Haskell record field names.
--
-- For a record named 'FooBar' with a field 'bazQuux', the Haskell
-- field will be named 'fooBarBazQuux'.
--
-- This is a decent enough heuristic with few collisions, but should
-- perhaps be configurable by the end-user.
genRecFieldNameV1 :: Name -> Name -> TH.Name
genRecFieldNameV1 (Name tn) (Name fn) =
  XTH.mkName_ (T.map Char.toLower (T.take 1 tn) <> T.drop 1 tn <> T.toTitle fn)

-- | Generate a regular type from a Machinator 'Type'.
genTypeV1 :: Type -> TH.Type
genTypeV1 ty =
  case ty of
    Variable (Name tn) ->
      XTH.conT (XTH.mkName_ tn)
    GroundT g ->
      case g of
        StringT ->
          XTH.conT (XTH.mkName_ "Text")
        BoolT ->
          XTH.conT (XTH.mkName_ "Bool")
        IntT ->
          XTH.conT (XTH.mkName_ "Int")
        LongT ->
          XTH.conT (XTH.mkName_ "Int64")
        DoubleT ->
          XTH.conT (XTH.mkName_ "Double")
        UUIDT ->
          XTH.conT (XTH.mkName_ "Data.UUID.UUID")
        DateT ->
          XTH.conT (XTH.mkName_ "Data.Time.Day")
        DateTimeT ->
          XTH.conT (XTH.mkName_ "Data.Time.LocalTime")
    ListT t2 ->
      XTH.listT_ (genTypeV1 t2)
    MaybeT t2 ->
      XTH.appT (XTH.conT (XTH.mkName_ "Maybe")) (genTypeV1 t2)
