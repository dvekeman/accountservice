{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Hadruki.Internal.Database where

import           Control.Monad.Logger               (runStderrLoggingT)
import qualified Data.ByteString                    as BS
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (Last (..))
import qualified Data.Pool                          as Pool
import qualified Data.Text.Encoding                 as T
import           Database.Persist.Postgresql

import           Hadruki.DatabaseHandle
import qualified Hadruki.Model.User as Model

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = do
    pool <- runStderrLoggingT $ createPostgresqlPool (connString config) 4

    let handle = Handle 
                  { hConfig = config
                  , hPool = pool
                  , findUserByUsername  = withPooledResource pool . Model.findUserByUsername
                  , findUserIdByActivationCode = \appName uid -> withPooledResource pool $ Model.findUserIdByActivationCode appName uid
                  , updateUserPassword  = \user password -> withPooledResource pool $ Model.updateUserPassword user password
                  , updateUserVerified  = \user appVerificationKey -> withPooledResource pool $ Model.updateUserVerified user appVerificationKey
                  , insertUser          = \appName user uid -> withPooledResource pool $ Model.insertUser appName user uid
                  , deleteUserByUsername = withPooledResource pool . Model.deleteUserByUsername
                  }
                  
    x <- f handle

    Pool.destroyAllResources pool
    return x

connString :: Config -> BS.ByteString
connString config = 
  let configConnectionString = getLast $ cConnectionString config
  in  fromMaybe BS.empty (T.encodeUtf8 <$> configConnectionString)

withPooledResource :: Pool.Pool SqlBackend -> SqlPersistM a -> IO a
withPooledResource pool q = 
  Pool.withResource pool $ \conn -> runQuery conn q
  
runQuery :: SqlBackend
            -> SqlPersistM a
            -> IO a
runQuery = flip runSqlPersistM

