{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Hadruki.Model.User
where

-------------------------------------------------------------------------------
import           Control.Monad               ( liftM )
import           Control.Monad.IO.Class      ( liftIO )
import qualified Data.Text                   as T
import           Data.Maybe
import           Database.Esqueleto
import           Hadruki.Model.Model
import qualified Hadruki.Model.App           as App
-------------------------------------------------------------------------------

findUserByUsername :: T.Text -> SqlPersistM ( Maybe UserAccount )
findUserByUsername username = do
  -- | Select all users where their username is equal to <username>
  liftIO $ putStrLn $ "findUserByUsername " ++ T.unpack username
  users <- select $
             from $ \u -> do
               where_ (u ^. UserAccountUsername ==. val username)
               return u
  liftIO $ putStrLn $ "Result " ++ show users
  case users of 
       []        -> return Nothing
       (user:_)  -> return $ fromUserEntity <$> Just user 

-------------------------------------------------------------------------------

findUserIdByActivationCode :: T.Text -> T.Text -> SqlPersistM (Maybe UserAccountId)
findUserIdByActivationCode appIdentifier uid = do
  results <- 
    select $
    from $ \(av `InnerJoin` app) -> do
      on (av ^. AppVerificationApp ==. app ^. AppId)
      where_ (   app ^. AppIdentifier ==. val appIdentifier
             &&. av  ^. AppVerificationVerificationKey ==. val uid
             )
      return (av, app)
  case results of 
       []        -> return Nothing
       ( ( appVerification, app ):_ )  ->
         -- Return the UserAccountId from the AppVerification entity 
         return $ Just . appVerificationUser $ fromAppVerificationEntity appVerification  

-------------------------------------------------------------------------------

-- updateUserPassword :: UserAccount -> T.Text -> SqlPersistM ()
updateUserPassword user password = do
  liftIO $ putStrLn $ "updateUserPassword " ++ T.unpack (userAccountUsername user)
  let username = userAccountUsername user
  update $ \u -> do
    set u [ UserAccountPassword =. val password ]
    where_ ( u ^. UserAccountUsername ==. val username )

-------------------------------------------------------------------------------

updateUserVerified :: UserAccountId -> T.Text -> SqlPersistM ()
updateUserVerified userId appVerificationKey = do
  liftIO $ putStrLn $ "updateUserVerified " ++ show userId
  update $ \av -> do
    set av [ AppVerificationVerified        =. val True 
           ]
    where_ ( av ^. AppVerificationUser ==. val userId )

-------------------------------------------------------------------------------

insertUser :: T.Text -> UserAccount -> T.Text -> SqlPersistM ( UserAccount, AppVerification )
insertUser appIdentifier user uid = do
  liftIO $ putStrLn $ "insertUser " ++ T.unpack (userAccountUsername user)
  userKey   <- insert user
  appKey    <- fromJust <$> App.findAppKeyByIdentifier appIdentifier
  appVerificationKey <- insert $ AppVerification userKey appKey uid False
  (,) <$> getJust userKey <*> getJust appVerificationKey

-------------------------------------------------------------------------------

deleteUserByUsername :: T.Text -> SqlPersistM ()
deleteUserByUsername username = do
  liftIO $ putStrLn $ "deleteUser " ++ T.unpack username
  delete $
     from $ \u ->
       where_ (u ^. UserAccountUsername ==. val username)
-------------------------------------------------------------------------------

fromUserEntity :: Entity UserAccount -> UserAccount
fromUserEntity (Entity _ user) = user

fromAppVerificationEntity :: Entity AppVerification -> AppVerification
fromAppVerificationEntity (Entity _ appVerification) = appVerification
