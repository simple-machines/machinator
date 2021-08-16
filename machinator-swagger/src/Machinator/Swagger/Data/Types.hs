{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Swagger.Data.Types (
    SwaggerTypesVersion (..)
  , SwaggerTypesError (..)
  , renderSwaggerTypesError
  ) where


import           P


data SwaggerTypesVersion
  = SwaggerTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data SwaggerTypesError
  = SwaggerTypesError
  deriving (Eq, Ord, Show)

renderSwaggerTypesError :: SwaggerTypesError -> Text
renderSwaggerTypesError pe =
  case pe of
    SwaggerTypesError ->
      "SwaggerTypesError"
