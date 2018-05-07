{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Hadruki.Model.App 
  ( findAppByName
  , findAppKeyByName
  
  , updateApp
  
  , insertApp
  
  , deleteAppByName
  )
where

-------------------------------------------------------------------------------
import           Control.Monad.IO.Class      ( liftIO )
import qualified Data.Text                   as T
import           Database.Esqueleto
import           Hadruki.Model.Model
-------------------------------------------------------------------------------

findAppByName :: T.Text -> SqlPersistM (Maybe App)
findAppByName name = do 
  app <- findAppByName' name
  return $ fromAppEntity <$> app
 
findAppKeyByName :: T.Text -> SqlPersistM (Maybe (Key App))
findAppKeyByName name = do 
  app <- findAppByName' name
  return $ fromAppEntity' <$> app

findAppByName' :: T.Text -> SqlPersistM (Maybe (Entity App))
findAppByName' name = do
  apps <- select $
          from $ \a -> do
            where_ (a ^. AppName ==. val name)
            return a
  case apps of 
       []        -> return Nothing
       (app:_)   -> return $ Just app 

-------------------------------------------------------------------------------

updateApp :: App -> SqlPersistM ()
updateApp App{..} =
  update $ \a -> do
    set a [ AppConfirmationCallback =. val appConfirmationCallback ]
    where_ ( a ^. AppName ==. val appName )

-------------------------------------------------------------------------------

insertApp :: T.Text -> T.Text -> SqlPersistM App
insertApp name confirmationCallback = do
  pk <- insert $ App name confirmationCallback
  getJust pk

-------------------------------------------------------------------------------

deleteAppByName :: T.Text -> SqlPersistM ()
deleteAppByName name =
  delete $
     from $ \a ->
       where_ (a ^. AppName ==. val name)
-------------------------------------------------------------------------------

fromAppEntity :: Entity App -> App
fromAppEntity (Entity _ app) = app

fromAppEntity' :: Entity App -> Key App
fromAppEntity' (Entity key _) = key
