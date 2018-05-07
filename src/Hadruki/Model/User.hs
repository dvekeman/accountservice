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

findUserIdByActivationCode :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
findUserIdByActivationCode appName uid = do
  results <- 
    select $
    from $ \(av `InnerJoin` app) -> do
    on (av ^. AppVerificationApp ==. app ^. AppId)
    where_ (   app ^. AppName ==. val appName
           &&. av  ^. AppVerificationVerificationKey ==. val uid
           )
    return (av, app)
  case results of 
       []        -> return Nothing
       ( ( appVerification, app ):_ )  ->
         -- Return the UserId from the AppVerification entity 
         return $ Just . appVerificationUser $ fromAppVerificationEntity appVerification  

-------------------------------------------------------------------------------

-- updateUserPassword :: User -> T.Text -> SqlPersistM ()
updateUserPassword user password = do
  liftIO $ putStrLn $ "updateUserPassword " ++ T.unpack (userUsername user)
  let username = userUsername user
  update $ \u -> do
    set u [ UserPassword =. val password ]
    where_ ( u ^. UserUsername ==. val username )

-------------------------------------------------------------------------------

updateUserVerified :: UserId -> T.Text -> SqlPersistM ()
updateUserVerified userId appVerificationKey = do
  liftIO $ putStrLn $ "updateUserVerified " ++ show userId
  update $ \av -> do
    set av [ AppVerificationVerified        =. val True 
           ]
    where_ ( av ^. AppVerificationUser ==. val userId )

-------------------------------------------------------------------------------

insertUser :: T.Text -> User -> T.Text -> SqlPersistM ( User, AppVerification )
insertUser appName user uid = do
  liftIO $ putStrLn $ "insertUser " ++ T.unpack (userUsername user)
  userKey   <- insert user
  appKey    <- fromJust <$> App.findAppKeyByName appName
  appVerificationKey <- insert $ AppVerification userKey appKey uid False
  (,) <$> getJust userKey <*> getJust appVerificationKey

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

fromAppVerificationEntity :: Entity AppVerification -> AppVerification
fromAppVerificationEntity (Entity _ appVerification) = appVerification
