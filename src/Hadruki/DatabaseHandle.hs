{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Hadruki.DatabaseHandle
    ( Handle (..)
    
    , Config (..)

    , Error (..)
    ) where

import           Control.Exception                  (Exception)
import qualified Data.Aeson                         as Aeson
import           Data.Monoid                        (Last (..))
import qualified Data.Pool                          as Pool
import qualified Data.Text                          as T
import           Database.Persist.Sql               (SqlBackend)
import           Hadruki.Model.Model

data Error
    = Constraint String
    | NotFound String

instance Show Error where
    show (Constraint msg) = msg
    show (NotFound   msg) = msg

instance Exception Error

newtype Config = Config
    { cConnectionString :: Last T.Text
    } deriving (Show)

instance Monoid Config where
    mempty                        = Config mempty
    mappend (Config l) (Config r) = Config (l `mappend` r)

instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Config" $ \o ->
        Config <$> o Aeson..: "connection_string"

data Handle = Handle
    { hConfig :: Config
    , hPool   :: Pool.Pool SqlBackend
    , findUserByUsername :: T.Text -> IO (Maybe User)
    , updateUserPassword :: User -> T.Text -> IO () 
    , updateUserVerified :: User -> IO () 
    , insertUser         :: User -> IO ()
    }