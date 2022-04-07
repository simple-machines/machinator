{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (IO, FilePath)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)

import           Machinator.Core as Machinator
import           Machinator.Scala as Machinator
import           Machinator.Scala.Data.Types ( renderScalaTypesError )

data Options
  = Options
    { targetDirectory :: Maybe FilePath  -- ^ Directory to write output files.
    , package :: Text                    -- ^ Scala package name.
    , sourceFiles :: [FilePath]          -- ^ Input files.
    }

inputs :: Parser [FilePath]
inputs = many (strArgument (metavar "machinator"))

options :: Parser Options
options = Options
  <$> optional (strOption (long "target" <> metavar "DIR"))
  <*> strOption (long "package" <> metavar "PKG")
  <*> inputs

main :: IO ()
main = do
  (Options mOut pkg files) <- execParser opts
  definitions <- orDie renderMachinatorError $ parseData files
  results     <- orDie renderScalaTypesError $ hoistEither $ typesV1 pkg definitions

  case mOut of
    Just out ->
      for_ results $ \(fp, contents) -> do
        let dest = out </> fp
        createDirectoryIfMissing True (takeDirectory dest)
        T.writeFile dest contents
        IO.putStrLn ("Created " <> dest )
    Nothing ->
      for_ results $ \(fp, contents) -> do
        IO.putStrLn ("File: " <> fp )
        IO.putStrLn (T.unpack contents)



  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

parseData :: [FilePath] -> EitherT Machinator.MachinatorError IO [Machinator.DefinitionFile]
parseData dfiles = do
  for dfiles  $ \dfile -> do
    t <- liftIO (T.readFile dfile)
    Machinator.Versioned _v defs <-
      hoistEither (Machinator.parseDefinitionFile dfile t)
    pure defs

