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
import           Machinator.Haskell as Machinator
import           Machinator.Haskell.Aeson.CodeGen (generateAesonModuleV1)



inputs :: Parser [FilePath]
inputs = many (strArgument (metavar "machinator"))

main :: IO ()
main = do
  files       <- execParser opts
  definitions <- orDie (T.pack . show) $ parseData files
  results     <- orDie (T.pack . show) $ hoistEither $ typesV1 definitions
  let aeson    = generateAesonModuleV1 (concatMap definitionFileDefinitions definitions)

  for_ results $ \(fp, contents) -> do
    IO.putStrLn fp
    IO.putStrLn "===="
    T.putStrLn contents

  IO.putStrLn ""
  IO.putStrLn ""

  T.putStrLn aeson


  where
    opts = info (inputs <**> helper)
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

