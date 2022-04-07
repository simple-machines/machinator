{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data.Definition (
  -- * Serialisation
    DefinitionFile (..)
  , Definition (..)
  , DefinitionFileGraph (..)
  -- * Datatype types
  , Docs (..)
  , Name (..)
  , Type (..)
  , Ground (..)
  , groundToName
  , groundFromName
  , DataType (..)
  -- * Traversals etc
  , free
  ) where


import           Data.Data (Data, Typeable)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as S

import           GHC.Generics (Generic)

import           P

import           System.IO  (FilePath)


-- | A set of type definitions from a given file.
data DefinitionFile
  = DefinitionFile {
      definitionFileName :: FilePath
    , definitionFileDefinitions :: [Definition]
    } deriving (Eq, Ord, Show, Data, Typeable, Generic)


newtype Docs = Docs {
    docText :: Text
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)


-- | A single data definition.
data Definition = Definition {
     defName :: Name
   , defDoc  :: Maybe Docs
   , defType :: DataType
   } deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The module graph.
-- Maps each file to the other files it depends on.
newtype DefinitionFileGraph = DefinitionFileGraph {
    unDefinitionFileGraph :: Map FilePath (Map FilePath (Set Name))
  } deriving (Eq, Ord, Show, Semigroup, Monoid, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------

-- | The name of a type.
newtype Name = Name {
    unName :: Text
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Types.
data Type
  = Variable Name
  | GroundT Ground
  | ListT Type
  | NonEmptyT Type
  | MaybeT Type
  | MapT Type Type
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Ground types, e.g. platform primitives.
data Ground
  = StringT
  | BoolT
  | IntT
  | LongT
  | DoubleT
  | UUIDT
  | DateT
  | DateTimeT
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Obtain the stringy form for a ground type.
groundToName :: Ground -> Name
groundToName g =
  case g of
    StringT ->
      Name "String"
    BoolT ->
      Name "Bool"
    IntT ->
      Name "Int"
    LongT ->
      Name "Long"
    DoubleT ->
      Name "Double"
    UUIDT ->
      Name "UUID"
    DateT ->
      Name "Date"
    DateTimeT ->
      Name "DateTime"

-- | Obtain the ground type for a stringy name.
groundFromName :: Alternative f => Name -> f Ground
groundFromName n =
  case unName n of
    "String" ->
      pure StringT
    "Bool" ->
      pure BoolT
    "Int" ->
      pure IntT
    "Long" ->
      pure LongT
    "Double" ->
      pure DoubleT
    "UUID" ->
      pure UUIDT
    "Date" ->
      pure DateT
    "DateTime" ->
      pure DateTimeT
    _ ->
      empty

-- | Declarable datatypes, e.g. sums or records.
data DataType
  = Variant (NonEmpty (Name, Maybe Docs, [(Name, Type)]))
  | Record [(Name, Type)]
  | Newtype (Name, Type)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- -----------------------------------------------------------------------------

free :: DataType -> Set Name
free d =
  case d of
    Variant nts ->
      fold . with nts $ \(_, _, fts) ->
        mconcat . with fts $ \(_, t) -> freeInType t
    Record fts ->
      mconcat . with fts $ \(_, t) -> freeInType t
    Newtype (_, t) ->
      mconcat [freeInType t]

freeInType :: Type -> Set Name
freeInType t =
  case t of
    Variable n ->
      S.singleton n
    GroundT _ ->
      S.empty
    ListT lt ->
      freeInType lt
    NonEmptyT lt ->
      freeInType lt
    MaybeT lt ->
      freeInType lt
    MapT lt rt ->
      freeInType lt `S.union` freeInType rt
