{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative

import           P

import           System.Directory
import           System.IO (IO, FilePath)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (dispatch, safeCommand, RunType (..), SafeCommand (..))

import           Machinator.Core as Machinator
import           Machinator.Scala as Machinator



imputs :: Parser [FilePath]
imputs = many (strArgument (metavar "machinator"))

main :: IO ()
main = do
  files <- execParser opts
  definitions <- orDie (T.pack . show) $ parseData files
  IO.print definitions


  where
    opts = info (imputs <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )


parseData :: [FilePath] -> EitherT Machinator.MachinatorError IO [Machinator.Definition]
parseData dfiles =
  fmap fold . for dfiles $ \dfile -> do
    t <- liftIO (T.readFile dfile)
    Machinator.Versioned _v (Machinator.DefinitionFile _fp defs) <-
      hoistEither (Machinator.parseDefinitionFile dfile t)
    pure defs
