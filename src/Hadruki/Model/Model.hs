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
    verificationKey    T.Text Maybe -- Used for resetting passwords
    verified Bool
    deriving Eq Show
    UniqueUserEmail email
    UniqueUserUsername username
    deriving Typeable

|]
