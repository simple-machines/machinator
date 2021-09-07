{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Python.Data.Types (
    PythonTypesVersion (..)
  , PythonTypesError (..)
  , renderPythonTypesError
  ) where


import           P


data PythonTypesVersion
  = PythonTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data PythonTypesError
  = PythonTypesError
  deriving (Eq, Ord, Show)

renderPythonTypesError :: PythonTypesError -> Text
renderPythonTypesError pe =
  case pe of
    PythonTypesError ->
      "PythonTypesError"
