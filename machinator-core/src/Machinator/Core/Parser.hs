{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Machinator.Core.Parser (
    parseDefinitionFile
  , ParseError (..)
  , renderParseError
  ) where


import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import qualified Data.Set as S
import qualified Data.Text as T

import           Machinator.Core.Data.Definition
import           Machinator.Core.Data.Position
import           Machinator.Core.Data.Token as MT
import           Machinator.Core.Data.Version

import           P

import           System.IO (FilePath)

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Error (errorBundlePretty)


data ParseError = ParseError Text
  deriving (Eq, Ord, Show)

renderParseError :: ParseError -> Text
renderParseError e =
  case e of
    ParseError t ->
      t

-- | Pure parser for definition files.
--
-- The 'FilePath' is for error reporting.
parseDefinitionFile :: FilePath -> Text -> Versioned [Positioned Token] -> Either ParseError (Versioned DefinitionFile)
parseDefinitionFile file contents (Versioned v ts) =
  first (ParseError . T.pack . errorBundlePretty) (M.runParser (parseVersioned file v <* M.eof) file (TokenStream contents ts))


-- -----------------------------------------------------------------------------

type Parser = M.Parsec ParseErrorComponent TokenStream

data TokenStream = TokenStream {
    myStreamInput :: Text -- for showing offending lines
  , unTokenStream :: [Positioned Token]
  } deriving (Eq, Ord, Show)

instance M.Stream TokenStream where
  type Token TokenStream =
    Positioned Token

  type Tokens TokenStream =
    [Positioned Token]

  tokenToChunk Proxy =
    pure
  tokensToChunk Proxy =
    id
  chunkToTokens Proxy =
    id
  chunkLength Proxy =
    length
  chunkEmpty Proxy =
    null
  take1_ (TokenStream _ [])
    = Nothing
  take1_ (TokenStream str (t:ts))
    = Just ( t , TokenStream str ts)

  takeN_ n (TokenStream str s)
    | n <= 0    = Just ([], TokenStream str s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in Just (x, TokenStream str s')

  takeWhile_ f (TokenStream str s) =
    let (x, s') = List.span f s
     in (x, TokenStream str s')


  showTokens _ =
    List.intercalate " " . NonEmpty.toList . fmap (show . extractPositioned)

  reachOffset o (M.PosState input offset sourcePos tabWidth _) =
    ( newSourcePos
    , fromMaybe "" thisLine
    , M.PosState
        { M.pstateInput = TokenStream (myStreamInput input) post
        , M.pstateOffset = max offset o
        , M.pstateSourcePos = newSourcePos
        , M.pstateTabWidth = tabWidth
        , M.pstateLinePrefix = ""
        }
    )
    where
      newSourcePos =
        case post of
          [] ->    sourcePos
          (x:_) -> toSourcePos $ extractStartPosition x

      post =
        drop (o - offset) (unTokenStream input)

      (!?) :: [a] -> Int -> Maybe a
      (!?) [] _ = Nothing
      (!?) (x:xs) n | n == 0 = return x
                    | n < 0 = Nothing
                    | otherwise = (!?) xs (n-1)

      thisLine = fmap T.unpack $
        T.lines (myStreamInput input) !? (M.unPos (M.sourceLine newSourcePos) - 1)

      toSourcePos :: Position -> M.SourcePos
      toSourcePos = \case
        Position srcLine srcCol file ->
          M.SourcePos file
            (M.mkPos srcLine)
            (M.mkPos srcCol)


data ParseErrorComponent
  = FeatureGuard MachinatorVersion MachinatorFeature
  deriving (Eq, Ord, Show)

instance M.ShowErrorComponent ParseErrorComponent where
  showErrorComponent (FeatureGuard v f) =
    "Can't use feature: " <> show f <> "; version: " <> show (versionToNumber v :: Int)

parseError :: ParseErrorComponent -> Parser a
parseError =
  M.customFailure

-- -----------------------------------------------------------------------------

{-

-- machinator @ v1

data Foo = Bar String | Baz String

data Bap = Bip Foo

record Quux = {
    a : Foo
  , b : Bap
  , c : Quux
  }

record Quib = {
    a : Foo
  , b : Bap
  , c : Quib
  }

-}

parseVersioned :: FilePath -> MachinatorVersion -> Parser (Versioned DefinitionFile)
parseVersioned file v = do
  ds <- many (definition v)
  pure (Versioned v (DefinitionFile file ds))


docComment :: Parser Docs
docComment = do
  TDoc x <- satisfy (\case TDoc _ -> True; _ -> False)
  pure (Docs x)


definition :: MachinatorVersion -> Parser Definition
definition v =
  optional docComment >>= definition' v


definition' :: MachinatorVersion -> Maybe Docs -> Parser Definition
definition' v doc =
      record v doc
  <|> variant v doc
  <|> newtype' v doc


variant :: MachinatorVersion -> Maybe Docs -> Parser Definition
variant v doc = do
  token TData
  hasFeature v HasVariants
  x <- ident
  token TEquals
  cs <- sepBy1 (alternative v) (token TChoice)
  pure (Definition x doc (Variant cs))

alternative :: MachinatorVersion -> Parser (Name, Maybe Docs, [(Name, Type)])
alternative v = do
  mDoc <- optional docComment
  name <- ident
  ts   <- optional $ do
    token TLBrace *> M.sepEndBy (recordField v) (token TComma) <* token TRBrace
  pure (name, mDoc, fold ts)

record :: MachinatorVersion -> Maybe Docs -> Parser Definition
record v doc = do
  token TRecord
  hasFeature v HasRecords
  x <- ident
  token TEquals
  token TLBrace
  fts <- M.sepEndBy (recordField v) (token TComma)
  token TRBrace
  pure (Definition x doc (Record fts))

newtype' :: MachinatorVersion -> Maybe Docs -> Parser Definition
newtype' v doc = do
  token TNewtype
  hasFeature v HasRecords
  x <- ident
  token TEquals
  token TLBrace
  ft <- recordField v
  token TRBrace
  pure (Definition x doc (Newtype ft))

recordField :: MachinatorVersion -> Parser (Name, Type)
recordField v = do
  name <- ident <|> dataAsIdent <|> recordAsIdent
  token TTypeSig
  ty <- types v
  pure (name, ty)

types :: MachinatorVersion -> Parser Type
types v = do
  optionalParens (types' v)

types' :: MachinatorVersion -> Parser Type
types' v = do
  x <- ident
  case x of
    Name "List" ->
      hasFeature v HasLists *> (ListT <$> types v)
    Name "NonEmpty" ->
      hasFeature v HasNels *> (NonEmptyT <$> types v)
    Name "Map" ->
      hasFeature v HasMaps *> (MapT <$> types v <*> types v)
    Name "Maybe" ->
      hasFeature v HasLists *> (MaybeT <$> types v)
    _ ->
      case groundFromName x of
        Just t ->
          pure (GroundT t)
        Nothing ->
          pure (Variable x)

parens :: Parser a -> Parser a
parens =
  M.between (token TLParen) (token TRParen)

optionalParens :: Parser a -> Parser a
optionalParens p =
  parens p <|> p

-- -----------------------------------------------------------------------------

hasFeature :: MachinatorVersion -> MachinatorFeature -> Parser ()
hasFeature v f =
  if featureEnabled v f then pure () else parseError (FeatureGuard v f)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy m sep =
  M.try (fmap toList (sepBy1 m sep)) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 m sep = do
  a <- m
  bs <- many (M.try sep *> m)
  pure (a :| bs)

ident :: Parser Name
ident = do
  TIdent x <- satisfy (\case TIdent _ -> True; _ -> False)
  pure (Name x)

recordAsIdent :: Parser Name
recordAsIdent = do
  TRecord <- satisfy (\case TRecord -> True; _ -> False)
  pure (Name MT.recordKeyword)

dataAsIdent :: Parser Name
dataAsIdent = do
  TData <- satisfy (\case TData -> True; _ -> False)
  pure (Name MT.dataKeyword)

token :: Token -> Parser ()
token t =
  satisfy (== t) *> pure ()

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = M.token testToken S.empty
  where
    testToken (a :@ _) =
      if f a then
        Just a
      else
        Nothing
