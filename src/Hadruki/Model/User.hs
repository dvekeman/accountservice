{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Hadruki.Model.User 
  ( findUserByUsername
  , updateUserPassword
  , updateUserVerified
  , insertUser
  )
where

-------------------------------------------------------------------------------
import           Control.Monad.IO.Class      ( liftIO )
import qualified Data.Text                   as T
import           Database.Esqueleto
import           Hadruki.Model.Model
-------------------------------------------------------------------------------

findUserByUsername :: T.Text -> SqlPersistM (Maybe User)
findUserByUsername username = do
  -- | Select all users where their username is equal to <username>
  liftIO $ putStrLn $ "findUserByUsername " ++ T.unpack username
  users <- select $
           from $ \u -> do
           where_ (u ^. UserUsername ==. val username)
           return u
  liftIO $ putStrLn $ "Result " ++ show users
  case users of 
       []        -> return Nothing
       (user:_)  -> return $ fromUserEntity <$> Just user 

-------------------------------------------------------------------------------

updateUserPassword :: User -> T.Text -> SqlPersistM ()
updateUserPassword user password = do
  liftIO $ putStrLn $ "updateUserPassword " ++ T.unpack (userUsername user)
  let username = userUsername user
  update $ \u -> do
    set u [ UserPassword =. val password ]
    where_ ( u ^. UserUsername ==. val username )

-------------------------------------------------------------------------------

updateUserVerified :: User -> SqlPersistM ()
updateUserVerified user = do
  liftIO $ putStrLn $ "updateUserVerified " ++ T.unpack (userUsername user)
  let username = userUsername user
  update $ \u -> do
    set u [ UserVerificationKey =. val Nothing
          , UserVerified        =. val True 
          ]
    where_ ( u ^. UserUsername ==. val username )

-------------------------------------------------------------------------------

insertUser :: User -> SqlPersistM ()
insertUser user = do
  liftIO $ putStrLn $ "insertUser " ++ T.unpack (userUsername user)
  insert_ user

-------------------------------------------------------------------------------

fromUserEntity :: Entity User -> User
fromUserEntity (Entity _ user) = user
