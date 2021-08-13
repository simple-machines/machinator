
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Example.Aggregate where


import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Functor
import           Data.Text (Text)

import           P (Int, Maybe (..), return)

data AggregateExpression = Sum | Average

data Feature
    = EventFeature !Text !AggregateExpression !FilterExpression
    | RowFeature

data FeatureSet
    = FeatureSet {featureSetX :: !Text, featureSetY :: !([Int])}

data FeatureStore
    = FeatureStore {featureStoreSet :: !FeatureSet,
                    featureStoreTime :: !Int,
                    featureStoreOptional :: !(Maybe Int)}


data FilterExpression
    = FilterExpression {filterExpressionSql :: !Text}


toJsonV1AggregateExpression :: AggregateExpression ->
                               Data.Aeson.Value
toJsonV1AggregateExpression = \x -> case x of
                                        Sum -> Data.Aeson.object ["tag" Data.Aeson..= ("Sum" :: Text)]
                                        Average -> Data.Aeson.object ["tag" Data.Aeson..= ("Average" :: Text)]
fromJsonV1AggregateExpression :: Data.Aeson.Value ->
                                 Data.Aeson.Types.Parser AggregateExpression
fromJsonV1AggregateExpression = \x -> Data.Aeson.Types.withObject "AggregateExpression" (\o -> do {tag <- o Data.Aeson..: "tag";
                                                                                                   case tag :: Text of
                                                                                                       "Sum" -> do return Sum
                                                                                                       "Average" -> do return Average
                                                                                                       _ -> Control.Monad.fail "AggregateExpression"}) x
toJsonV1Feature :: Feature -> Data.Aeson.Value
toJsonV1Feature = \x -> case x of
                            EventFeature f1
                                         f2
                                         f3 -> Data.Aeson.object ["tag" Data.Aeson..= ("EventFeature" :: Text),
                                                                  "name" Data.Aeson..= Data.Aeson.toJSON f1,
                                                                  "aggregate" Data.Aeson..= toJsonV1AggregateExpression f2,
                                                                  "filter" Data.Aeson..= toJsonV1FilterExpression f3]
                            RowFeature -> Data.Aeson.object ["tag" Data.Aeson..= ("RowFeature" :: Text)]
fromJsonV1Feature :: Data.Aeson.Value ->
                     Data.Aeson.Types.Parser Feature
fromJsonV1Feature = \x -> Data.Aeson.Types.withObject "Feature" (\o -> do {tag <- o Data.Aeson..: "tag";
                                                                           case tag :: Text of
                                                                               "EventFeature" -> do {name <- (o Data.Aeson..: "name") Control.Monad.>>= Data.Aeson.parseJSON;
                                                                                                     aggregate <- (o Data.Aeson..: "aggregate") Control.Monad.>>= fromJsonV1AggregateExpression;
                                                                                                     filter <- (o Data.Aeson..: "filter") Control.Monad.>>= fromJsonV1FilterExpression;
                                                                                                     return (EventFeature name aggregate filter)}
                                                                               "RowFeature" -> do return RowFeature
                                                                               _ -> Control.Monad.fail "Feature"}) x
toJsonV1FeatureSet :: FeatureSet -> Data.Aeson.Value
toJsonV1FeatureSet = \x -> case x of
                               FeatureSet x
                                          y -> Data.Aeson.object ["x" Data.Aeson..= Data.Aeson.toJSON x,
                                                                  "y" Data.Aeson..= Data.Functor.fmap Data.Aeson.toJSON y]
fromJsonV1FeatureSet :: Data.Aeson.Value ->
                        Data.Aeson.Types.Parser FeatureSet
fromJsonV1FeatureSet = \x -> Data.Aeson.Types.withObject "FeatureSet" (\o -> do {x <- (o Data.Aeson..: "x") Control.Monad.>>= Data.Aeson.parseJSON;
                                                                                 y <- (o Data.Aeson..: "y") Control.Monad.>>= Control.Monad.mapM Data.Aeson.parseJSON;
                                                                                 return (FeatureSet x y)}) x
toJsonV1FeatureStore :: FeatureStore -> Data.Aeson.Value
toJsonV1FeatureStore = \x -> case x of
                                 FeatureStore set
                                              time
                                              optional -> Data.Aeson.object ["set" Data.Aeson..= toJsonV1FeatureSet set,
                                                                             "time" Data.Aeson..= Data.Aeson.toJSON time,
                                                                             "optional" Data.Aeson..= Data.Functor.fmap Data.Aeson.toJSON optional]
fromJsonV1FeatureStore :: Data.Aeson.Value ->
                          Data.Aeson.Types.Parser FeatureStore
fromJsonV1FeatureStore = \x -> Data.Aeson.Types.withObject "FeatureStore" (\o -> do {set <- (o Data.Aeson..: "set") Control.Monad.>>= fromJsonV1FeatureSet;
                                                                                     time <- (o Data.Aeson..: "time") Control.Monad.>>= Data.Aeson.parseJSON;
                                                                                     optional <- (o Data.Aeson..: "optional") Control.Monad.>>= Control.Monad.mapM Data.Aeson.parseJSON;
                                                                                     return (FeatureStore set time optional)}) x
toJsonV1FilterExpression :: FilterExpression -> Data.Aeson.Value
toJsonV1FilterExpression = \x -> case x of
                                     FilterExpression sql -> Data.Aeson.object ["sql" Data.Aeson..= Data.Aeson.toJSON sql]
fromJsonV1FilterExpression :: Data.Aeson.Value ->
                              Data.Aeson.Types.Parser FilterExpression
fromJsonV1FilterExpression = \x -> Data.Aeson.Types.withObject "FilterExpression" (\o -> do {sql <- (o Data.Aeson..: "sql") Control.Monad.>>= Data.Aeson.parseJSON;
                                                                                             return (FilterExpression sql)}) x
