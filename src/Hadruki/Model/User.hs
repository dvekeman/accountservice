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
  , findUserByActivationCode
  
  , updateUserPassword
  , updateUserVerified
  
  , insertUser
  
  , deleteUserByUsername
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

findUserByActivationCode :: T.Text -> SqlPersistM (Maybe User)
findUserByActivationCode uid = do
  -- | Select all users where their activation code is equal to <uid>
  liftIO $ putStrLn $ "findUserByActivationCode " ++ T.unpack uid
  users <- select $
           from $ \u -> do
           where_ 
             (   u ^. UserVerificationKey ==. just (val uid)
             &&. u ^. UserVerified        ==. val False
             )
           return u
  liftIO $ putStrLn $ "Result " ++ show users
  case users of 
       []        -> return Nothing
       (user:_)  -> return $ fromUserEntity <$> Just user 

-------------------------------------------------------------------------------

-- updateUserPassword :: User -> T.Text -> SqlPersistM ()
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

insertUser :: User -> SqlPersistM User
insertUser user = do
  liftIO $ putStrLn $ "insertUser " ++ T.unpack (userUsername user)
  pk <- insert user
  getJust pk

-------------------------------------------------------------------------------

deleteUserByUsername :: T.Text -> SqlPersistM ()
deleteUserByUsername username = do
  liftIO $ putStrLn $ "deleteUser " ++ T.unpack username
  delete $
     from $ \u ->
       where_ (u ^. UserUsername ==. val username)
-------------------------------------------------------------------------------

fromUserEntity :: Entity User -> User
fromUserEntity (Entity _ user) = user
