module Hadruki.Model
    ( module Hadruki.DatabaseHandle
    , module Hadruki.Model.Model
    , createTables
    ) where

import           Database.Persist.Postgresql

import           Hadruki.Model.Model
import           Hadruki.DatabaseHandle

createTables :: Handle -> IO ()
createTables h = do
    let pool = hPool h
    runSqlPersistMPool (runMigration migrateAll) pool 
