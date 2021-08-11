{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data.Token (
    Token (..)
  , recordKeyword
  , newtypeKeyword
  , dataKeyword
  ) where


import           P


data Token
  = TData
  | TIdent Text
  | TEquals
  | TChoice
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TTypeSig
  | TRecord
  | TNewType
  deriving (Eq, Ord, Show)


recordKeyword :: Text
recordKeyword =
  "record"

dataKeyword :: Text
dataKeyword =
  "data"

newtypeKeyword :: Text
newtypeKeyword =
  "newtype"
