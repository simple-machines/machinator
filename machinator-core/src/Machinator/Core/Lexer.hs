{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Lexer (
    lexVersioned
  , LexError (..)
  , renderLexError
  ) where


import qualified Data.Text as T
import           Data.Void (Void)

import           Machinator.Core.Data.Position
import           Machinator.Core.Data.Token
import           Machinator.Core.Data.Version

import           P

import           System.IO  (FilePath)

import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.Megaparsec.Byte as C

type Parser = M.Parsec Void Text

data LexError
  = LexError Text
  deriving (Eq, Ord, Show)

renderLexError :: LexError -> Text
renderLexError e =
  case e of
    LexError t ->
      t

lexVersioned :: FilePath -> Text -> Either LexError (Versioned [Positioned Token])
lexVersioned file t =
  first (LexError . T.pack . errorBundlePretty) (M.runParser (lexVersioned' <* M.eof) file t)


-- -----------------------------------------------------------------------------


lexVersioned' :: Parser (Versioned [Positioned Token])
lexVersioned' = do
  mv <- version
  comment mv
  ts <- many (token mv)
  space
  pure (Versioned mv ts)

version :: Parser MachinatorVersion
version = do
  string "-- machinator @ v"
  v <- ML.decimal
  _ <- M.newline
  space
  case versionFromNumber (v :: Int) of
    Just ver ->
      pure ver
    Nothing ->
      -- TODO custom error component for bad version number?
      fail ("Unknown version number " <> show v <> " - expected 1")

token :: MachinatorVersion -> Parser (Positioned Token)
token v =
  token' <* space <* comment v

token' :: Parser (Positioned Token)
token' =
  withPosition $ M.choice [
      M.try $
        string (T.unpack dataKeyword) *> M.spaceChar
          $> TData
    , M.try $
        string (T.unpack recordKeyword) *> M.spaceChar
          $> TRecord
    , M.try $
        string (T.unpack newtypeKeyword) *> M.spaceChar
          $> TNewtype
    , docComment
    , string "=" $> TEquals
    , string "|" $> TChoice
    , string "(" $> TLParen
    , string ")" $> TRParen
    , string "{" $> TLBrace
    , string "}" $> TRBrace
    , string ":" $> TTypeSig
    , string "," $> TComma
    , ident
    ]

comment :: MachinatorVersion -> Parser ()
comment v =
  when (featureEnabled v HasComments) $
    void . many $ M.choice [
        skipNotDocBlockCommentNested
      , singleNotDocLineComment
      , void M.spaceChar
      ]

docComment :: Parser Token
docComment =
  singleDocComment <|> multiDocComment

singleDocComment :: Parser Token
singleDocComment = do
  M.try (string "--" <* M.space <* M.char '|')
  s <- M.takeWhileP (Just "character") (/= '\n')
  pure (TDoc s)

singleNotDocLineComment :: Parser ()
singleNotDocLineComment = do
  M.try (string "--" *> M.notFollowedBy (M.space *> C.string "|"))
  void (M.takeWhileP (Just "character") (/= '\n'))

multiDocComment :: Parser Token
multiDocComment = do
  M.try (string "{-" <* M.space <* M.char '|')
  s <- M.manyTill M.anySingle (C.string "-}")
  pure (TDoc (T.pack s))
{-# INLINEABLE multiDocComment #-}

skipNotDocBlockCommentNested :: Parser ()
skipNotDocBlockCommentNested = p *> void (M.manyTill e n)
  where
    e = ML.skipBlockCommentNested "{-" "-}" <|> void M.anySingle
    p = M.try (C.string "{-" *> M.notFollowedBy (M.space *> C.string "|"))
    n = C.string "-}"
{-# INLINEABLE skipNotDocBlockCommentNested #-}

ident :: Parser Token
ident = do
  s <- some (M.char '_' <|> M.alphaNumChar)
  pure (TIdent (T.pack s))

-- -----------------------------------------------------------------------------

space :: Parser ()
space =
  many M.spaceChar *> pure ()
{-# INLINEABLE space #-}

string :: [Char] -> Parser ()
string s =
  M.string (T.pack s) *> pure ()
{-# INLINEABLE string #-}

getPosition :: Parser Position
getPosition =
  fmap posPosition M.getSourcePos
{-# INLINEABLE getPosition #-}

withPosition :: Parser a -> Parser (Positioned a)
withPosition p = do
  a <- getPosition
  b <- p
  c <- getPosition
  pure (b :@ Range a c)
{-# INLINEABLE withPosition #-}

posPosition :: M.SourcePos -> Position
posPosition (M.SourcePos file line col) =
  Position (fromIntegral (M.unPos line)) (fromIntegral (M.unPos col)) file
{-# INLINEABLE posPosition #-}
