{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Scala.Data.Types (
    ScalaTypesVersion (..)
  , ScalaTypesError (..)
  , renderScalaTypesError
  ) where


import           P


data ScalaTypesVersion
  = ScalaTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data ScalaTypesError
  = ScalaTypesError
  deriving (Eq, Ord, Show)

renderScalaTypesError :: ScalaTypesError -> Text
renderScalaTypesError pe =
  case pe of
    ScalaTypesError ->
      "ScalaTypesError"
