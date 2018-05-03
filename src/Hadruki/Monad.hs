{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hadruki.Monad where

import           Control.Applicative                       ( (<|>), empty )
import           Control.Monad.Reader                      ( ReaderT, asks )
import           Control.Monad.IO.Class
import           Data.Aeson                                ( (.:?), withObject )
import qualified Data.Aeson                                as Aeson
import           Data.String                               ( IsString )
import           Data.Maybe                                ( fromMaybe )
import qualified Data.Text                                 as T
import           Data.Text                                 ( Text )
import qualified Data.Text.Extended                        as T
import           Servant

import qualified Hadruki.Database                          as Database
import qualified Hadruki.Logger                            as Logger
import qualified Hadruki.Aws                               as Aws

import           GHC.Generics

type HadrukiM = ReaderT Handle Handler

data Config = Config { cScheme       :: Maybe T.Text
                     , cHost         :: Maybe [T.Text]
                     , cPort         :: Maybe Int
                     }
    deriving (Show)

instance Monoid Config where
    mempty = Config empty empty empty
    mappend c1 c2 = Config { cScheme = cScheme c1 <|> cScheme c2
                           , cHost = cHost c1 <|> cHost c2
                           , cPort = cPort c1 <|> cPort c2
                           }

instance Aeson.FromJSON Config where
    parseJSON = withObject "FromJSON Hadruki.Web.Config" $
        \o -> Config
            <$> o .:? "scheme"
            <*> o .:? "host"
            <*> o .:? "port"

data Handle = Handle { hConfig   :: Config
                     , hLogger   :: Logger.Handle
                     , hDatabase :: Database.Handle
                     , hAws      :: Aws.Handle
                     }

withHandle :: Config
           -> Logger.Handle
           -> Database.Handle
           -> Aws.Handle
           -> (Handle -> IO a)
           -> IO a
withHandle config logger database aws f =
    f $ Handle config logger database aws 

askHost :: HadrukiM T.Text
askHost = asks $ askHost' . fromMaybe [] . cHost . hConfig

askHost' :: IsString a => [a] -> a
ashHost' [] = "localhost"
askHost' [x] = x
askHost' (x:xs) = x

askPort :: HadrukiM Int
askPort = asks $ fromMaybe 8787 . cPort . hConfig

askPort' :: HadrukiM Text
askPort' = T.showInt <$> askPort

askScheme :: HadrukiM T.Text
askScheme = asks $ fromMaybe "http" . cScheme . hConfig

askServerUrl :: HadrukiM T.Text
askServerUrl = askServerUrl' =<< askHost

askServerUrl' :: T.Text -> HadrukiM T.Text
askServerUrl' address = do
  defaultPort <- askPort'
  let (host,port) = case T.splitOn ":" address of 
                              [host]      -> (host, defaultPort)
                              [host,port] -> (host, port)
  T.concat <$> sequence [ askScheme
                        , return "://"
                        , return host
                        , return ":"
                        , return port
                        ]

serverUrl :: Maybe String -> HadrukiM T.Text
serverUrl ( Just host ) = askServerUrl' $ T.pack host
serverUrl    Nothing    = askServerUrl

logError s = asks hLogger >>= liftIO . flip Logger.error' s
logInfo  s = asks hLogger >>= liftIO . flip Logger.info' s
logInfoT t = asks hLogger >>= liftIO . flip Logger.info' (T.unpack t)
logDebug s = asks hLogger >>= liftIO . flip Logger.debug' s
