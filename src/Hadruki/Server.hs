module Hadruki.Server
    ( Config (..)

    , runServer
    ) where

import           Control.Monad.Logger     (runStderrLoggingT)
import qualified Data.Aeson               as A
import           Data.Monoid              ((<>))
import qualified Data.Yaml                as Yaml
import qualified Hadruki.Logger           as Logger
import qualified Hadruki.Database         as Database
import qualified Hadruki.Aws              as Aws
import qualified Hadruki.Web as Web
import           System.Environment       (getArgs, getProgName)
import           System.Exit              (exitFailure)
import qualified System.IO                as IO

data Config = Config
    { cLogger     :: Logger.Config
    , cWeb        :: Web.Config
    , cDatabase   :: Database.Config
    , cAws        :: Aws.Config
    }

instance Monoid Config where
    mempty = Config
        { cLogger     = mempty
        , cWeb        = mempty
        , cDatabase   = mempty
        , cAws        = mempty
        }

    mappend l r = Config
        { cLogger     = cLogger     l <> cLogger     r
        , cWeb        = cWeb        l <> cWeb        r
        , cDatabase   = cDatabase   l <> cDatabase   r
        , cAws        = cAws        l <> cAws        r
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Hadruki.Main.Server.Config" $ \o ->
        Config
            <$> o A..:? "logger"      A..!= mempty
            <*> o A..:? "web"         A..!= mempty
            <*> o A..:? "database"    A..!= mempty
            <*> o A..:? "aws"         A..!= mempty

runServer :: IO ()
runServer = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [configPath] -> run configPath
        _ -> do
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <conf>"
            exitFailure

run :: FilePath -> IO ()
run configPath = do
    IO.hPutStrLn IO.stderr "Booting hadruki accountservice v0.1"

    errOrConfig <- Yaml.decodeFileEither configPath
    Config {..} <- either (fail . show) return errOrConfig

    Logger.withHandle cLogger $ \logger ->
        Database.withHandle cDatabase $ \db ->
          Aws.withHandle cAws $ \aws -> 
            Web.withHandle cWeb logger db aws $ \web ->
              Web.run web
