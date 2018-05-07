{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-} -- allow json in data model
{-# LANGUAGE DeriveDataTypeable         #-} -- deriving Typeable
module Hadruki.Model.Model where

import qualified Data.Text                          as T
import           Data.Typeable (Typeable)
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|

User json
    username  T.Text
    email     T.Text
    password  T.Text
    UniqueUserEmail email
    UniqueUserUsername username
    deriving Eq Show Typeable

App json
    identifier T.Text
    name       T.Text
    scheme     T.Text
    host       T.Text
    port       Int
    confirmationSuccessCallback T.Text
    confirmationFailCallback T.Text
    UniqueAppIdentifier
    deriving Eq Show Typeable

AppVerification json
    user     UserId
    app      AppId
    verificationKey    T.Text -- Used for resetting passwords
    verified           Bool   -- Used for resetting passwords
    deriving Eq Show Typeable

|]
