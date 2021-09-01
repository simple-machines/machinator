{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative

import           P

import           System.Directory
import           System.FilePath ((</>), takeDirectory, dropExtension)
import           System.IO (IO, FilePath)
import qualified System.IO as IO
import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)

import           Machinator.Core as Machinator
import           Machinator.Python as Machinator


data Options
  = Options
    { targetDirectory :: FilePath  -- ^ Directory to write output files.
    , sourceFiles :: [FilePath]    -- ^ Input files.
    }

inputs :: Parser [FilePath]
inputs = many (strArgument (metavar "machinator"))

options :: Parser Options
options = Options
  <$> strOption (long "target" <> metavar "DIR")
  <*> inputs


main :: IO ()
main = do
  (Options out files) <- execParser opts
  definitions <- orDie (T.pack . show) $ parseData files
  results     <- orDie (T.pack . show) $ hoistEither $ typesV1 definitions

  for_ results $ \(fp, contents) -> do
    let dest = dropExtension (out </> fp) </> "__init__.py"
    createDirectoryIfMissing True (takeDirectory dest)
    T.writeFile dest contents
    IO.putStrLn ("Created " <> dest)

  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Generate a Python client library from machinator source"
     <> header "gen-python - generate Python client implementation" )

parseData :: [FilePath] -> EitherT Machinator.MachinatorError IO [Machinator.DefinitionFile]
parseData dfiles = do
  for dfiles  $ \dfile -> do
    t <- liftIO (T.readFile dfile)
    Machinator.Versioned _v defs <-
      hoistEither (Machinator.parseDefinitionFile dfile t)
    pure defs

