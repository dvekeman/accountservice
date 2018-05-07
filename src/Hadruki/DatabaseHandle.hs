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
    -- UserAccount
    , findUserByUsername :: T.Text -> IO ( Maybe UserAccount )
    , findUserIdByActivationCode :: T.Text -> T.Text -> IO ( Maybe UserAccountId )
    , updateUserPassword :: UserAccount -> T.Text -> IO () 
    , updateUserVerified :: UserAccountId -> T.Text -> IO () 
    , insertUser         :: T.Text -> UserAccount -> T.Text -> IO ( UserAccount, AppVerification )
    , deleteUserByUsername         :: T.Text -> IO ()
    -- App
    , findAppByIdentifier :: T.Text -> IO ( Maybe App )
    -- 
    }
