{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative

import           P

import           System.IO (IO, FilePath)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)

import           Machinator.Core as Machinator
import           Machinator.Swagger as Machinator



inputs :: Parser [FilePath]
inputs = many (strArgument (metavar "machinator"))

main :: IO ()
main = do
  files       <- execParser opts
  definitions <- orDie (T.pack . show) $ parseData files
  let swagger  = fullSwagger definitions

  Char8.putStrLn . encodePretty $ swagger

  where
    opts = info (inputs <**> helper)
      ( fullDesc
     <> progDesc "Generate Swagger documentation for machinator definitions." )

parseData :: [FilePath] -> EitherT Machinator.MachinatorError IO [Machinator.DefinitionFile]
parseData dfiles = do
  for dfiles  $ \dfile -> do
    t <- liftIO (T.readFile dfile)
    Machinator.Versioned _v defs <-
      hoistEither (Machinator.parseDefinitionFile dfile t)
    pure defs

