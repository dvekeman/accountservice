{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Hadruki.Model.App 
  ( findAppByIdentifier
  , findAppKeyByIdentifier
  
  , updateApp
  
  , insertApp
  
  , deleteAppByIdentifier
  )
where

-------------------------------------------------------------------------------
import           Control.Monad.IO.Class      ( liftIO )
import qualified Data.Text                   as T
import           Database.Esqueleto
import           Hadruki.Model.Model
-------------------------------------------------------------------------------

findAppByIdentifier :: T.Text -> SqlPersistM (Maybe App)
findAppByIdentifier identifier = do 
  app <- findAppByIdentifier' identifier
  return $ fromAppEntity <$> app
 
findAppKeyByIdentifier :: T.Text -> SqlPersistM (Maybe (Key App))
findAppKeyByIdentifier identifier = do 
  app <- findAppByIdentifier' identifier
  return $ fromAppEntity' <$> app

findAppByIdentifier' :: T.Text -> SqlPersistM (Maybe (Entity App))
findAppByIdentifier' identifier = do
  apps <- select $
            from $ \a -> do
              where_ (a ^. AppIdentifier ==. val identifier)
              return a
  case apps of 
       []        -> return Nothing
       (app:_)   -> return $ Just app 

-------------------------------------------------------------------------------

updateApp :: App -> SqlPersistM ()
updateApp App{..} =
  update $ \a -> do
    set a [ AppScheme                      =. val appScheme
          , AppHost                        =. val appHost
          , AppPort                        =. val appPort
          , AppConfirmationSuccessCallback =. val appConfirmationSuccessCallback 
          , AppConfirmationFailCallback    =. val appConfirmationFailCallback
          ]
          
    where_ ( a ^. AppIdentifier ==. val appIdentifier )

-------------------------------------------------------------------------------

insertApp :: T.Text -- ^ Identifier 
          -> T.Text -- ^ Name
          -> T.Text -- ^ Scheme
          -> T.Text -- ^ Host
          -> Int    -- ^ Port 
          -> T.Text -- ^ Confirmation success callback
          -> T.Text -- ^ Confirmation fail callback
          -> SqlPersistM App
insertApp identifier name scheme host port confirmationSuccessCallback confirmationFailCallback = do
  pk <- insert $ App identifier name scheme host port confirmationSuccessCallback confirmationFailCallback
  getJust pk

-------------------------------------------------------------------------------

deleteAppByIdentifier :: T.Text -> SqlPersistM ()
deleteAppByIdentifier identifier =
  delete $
    from $ \a ->
    where_ (a ^. AppIdentifier ==. val identifier)
-------------------------------------------------------------------------------

fromAppEntity :: Entity App -> App
fromAppEntity (Entity _ app) = app

fromAppEntity' :: Entity App -> Key App
fromAppEntity' (Entity key _) = key
