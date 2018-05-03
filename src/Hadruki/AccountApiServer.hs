{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hadruki.AccountApiServer where

import           Control.Applicative      ( (<|>), empty )
import           Control.Exception           ( SomeException(..) )
import           Control.Monad.Catch         ( catch )
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader     ( ReaderT, asks, runReaderT )
import           Crypto.JOSE.JWK          ( JWK )
import           Crypto.PasswordStore     ( verifyPassword, makePassword )
import           Data.Aeson ((.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Monoid ((<>))
import qualified Data.Text               as T
import           Data.Text (Text)
import qualified Data.Text.Encoding      as Enc8
import qualified Data.Text.Extended      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LEnc8
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID

import           Servant
import           Servant.API
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan       ()

import           GHC.Generics

import           Hadruki.Monad
import           Hadruki.AccountApi
import           Hadruki.ApiUtil
import           Hadruki.Mail.AccountConfirmation
import qualified Hadruki.Model             as Model
import qualified Hadruki.Logger   as Logger
import qualified Hadruki.Database as Database
import           Hadruki.Internal.SignupRequest
import           Hadruki.Internal.ChangePasswordRequest
import           Hadruki.Internal.SignupResponse
import           Hadruki.Internal.LoginRequest
import           Hadruki.Internal.LoginResponse
import           Hadruki.Internal.LoginToken

apiServer :: CookieSettings -> JWTSettings -> ServerT (Api auths) HadrukiM
apiServer cs jwts = signupUser     cs jwts 
               :<|> loginUser      cs jwts 
               :<|> verifyAccount  cs jwts 
               :<|> changePassword cs jwts 

loginUser ::
     CookieSettings
  -> JWTSettings
  -> LoginRequest
  -> HadrukiM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
loginUser cs jwts (LoginRequest username password) = do
  -- 1. Verify the password
  verifyUserPassword username password
  -- 2. Get the user email
  email <- findUserEmail username
  -- 3. (Re)set the cookies 
  applyCookies cs jwts username email $ LoginResponse username email

verifyAccount ::
     CookieSettings
  -> JWTSettings
  -> Maybe String
  -> Maybe T.Text
  -> Maybe T.Text
  -> HadrukiM ( Headers '[ Header "Location" T.Text ] NoContent )
verifyAccount cs jwts mhost username verificationKey = do
  serverUrl <- serverUrl mhost
  -- 1. Find the user by username and uid
  muser <- findUserByUsernameAndUid_ username verificationKey
  case muser of 
    Just user -> 
      return $ Servant.addHeader (serverUrl <> "/static/index.html#login") NoContent
    Nothing   -> 
      return $ Servant.addHeader (serverUrl <> "/static/index.html#invalid-verification") NoContent

verifyUserAccount :: 
     CookieSettings
  -> JWTSettings
  -> T.Text
  -> HadrukiM (Headers '[Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie ] LoginResponse )
verifyUserAccount cs jwts username = do
  -- 1. Update the user
  setUserVerified username
  -- 2. Get the user email
  email <- findUserEmail username
  -- 3. (Re)set the cookies 
  applyCookies cs jwts username email $ LoginResponse username email 
  
signupUser ::
     CookieSettings
  -> JWTSettings
  -> SignupRequest
  -> HadrukiM SignupResponse
signupUser cs jwts (SignupRequest username email password) = do
  serverUrl <- askServerUrl
  -- 1. Create the password
  hashedPassword <- makeUserPassword password
  -- 2. Insert the user
  createUser username email hashedPassword
  -- 3. Send an email (TODO: async // TODO: url)
  sendConfirmationEmail username email $ serverUrl <> "/static/index.html#account-verified"
  return $ SignupResponse username email

changePassword ::
     CookieSettings
  -> JWTSettings
  -> AuthResult LoginToken
  -> ChangePasswordRequest
  -> HadrukiM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
changePassword cs jwts (Authenticated (LoginToken tokenUsername tokenEmail)) (ChangePasswordRequest username password newpassword) = do
  -- 1. Verify the password
  verifyUserPassword username password
  -- 2. Change the password
  changeUserPassword username newpassword
  email <- findUserEmail username
  -- 4. (Re)set the cookies 
  applyCookies cs jwts username email $ LoginResponse username email
changePassword cs jwts _ _ = throwError unauthorized

findUserEmail :: T.Text -> HadrukiM T.Text
findUserEmail username =
  withUser username $ \mUser db ->
    case mUser of 
      Just user -> return $ Model.userEmail user
      -- TODO: throw error?
      Nothing   -> return ""


{-| Change the password for the user with the provided username 
-}
changeUserPassword :: T.Text -> T.Text -> HadrukiM()
changeUserPassword username newpassword = do
  newHashedpwd <- liftIO $ Enc8.decodeUtf8 <$> makePassword (Enc8.encodeUtf8 newpassword) 17
  withUser username $ \mUser db ->
    case mUser of 
      Just user -> liftIO $ Model.updateUserPassword db user newHashedpwd
      Nothing   -> return ()

{-|  
-}
setUserVerified :: T.Text -> HadrukiM()
setUserVerified username =
  withUser username $ \mUser db ->
    case mUser of 
      Just user -> liftIO $ Model.updateUserVerified db user
      Nothing   -> return ()

{-| Verifies the password and throws HTTP 401 
    if the user is not found or 
    if the password does not match
-}
verifyUserPassword :: T.Text -> T.Text -> HadrukiM ()
verifyUserPassword username password = 
  withUser username $ \user db -> 
    case doVerifyPassword password <$> user of
      Just True -> return ()
      _         -> throwError unauthorized
  
  where doVerifyPassword password user = 
          verifyPassword (Enc8.encodeUtf8 password) (Enc8.encodeUtf8 $ Model.userPassword user)

{-| Generate the user password
-}
makeUserPassword :: T.Text -> HadrukiM T.Text
makeUserPassword password = 
  liftIO $ Enc8.decodeUtf8 <$> makePassword (Enc8.encodeUtf8 password) 17

{-| Create user with username and password 
-}
createUser :: T.Text -> T.Text -> T.Text -> HadrukiM ()
createUser username email password = do
  db <- asks hDatabase
  uid <- UUID.toText <$> liftIO UUID.nextRandom
  liftIO 
    (Model.insertUser db (Model.User username email password (Just uid) False))
      `catch` (\(SomeException e) -> do 
        logError $ show e
        throwError . failedTo $ 
          "Create account failed for username '" <> asLazyBS username <> "' and email '" <> asLazyBS email <> "'")

applyCookies :: CookieSettings
             -> JWTSettings
             -> T.Text
             -> T.Text
             -> (T.Text -> LoginResponse)
             -> HadrukiM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
applyCookies cs jwts username email response = do
  let loginToken = LoginToken username email
  -- TODO: Replace Nothing by Maybe UTCTime
      expiryDate = Nothing
  etoken <- liftIO $ makeJWT loginToken jwts expiryDate
  case etoken of
    Right token -> do
      let loginUser = response (encodeToken token)
      mApplyCookies <- liftIO $ acceptLogin cs jwts loginToken
      case mApplyCookies of
        Nothing -> throwError failedToGenerateJWT
        Just applyCookies -> return $ applyCookies loginUser
    Left err -> do
      logError (show err)
      throwError failedToGenerateJWT

findUserByUsernameAndUid_ :: Maybe T.Text -> Maybe T.Text -> HadrukiM (Maybe Model.User)
findUserByUsernameAndUid_ Nothing _ = return Nothing
findUserByUsernameAndUid_ _ Nothing = return Nothing
findUserByUsernameAndUid_ (Just username) (Just uid) = findUserByUsernameAndUid username uid

findUserByUsernameAndUid :: T.Text -> T.Text -> HadrukiM (Maybe Model.User)
findUserByUsernameAndUid username uid = 
  withUser username $ \user db ->
    return $ uidMatches uid =<< user

uidMatches :: T.Text -> Model.User -> Maybe Model.User
uidMatches uid user@Model.User{..} 
  | userVerificationKey == Just uid = Just user
  | otherwise                       = Nothing 

withUser :: T.Text -> (Maybe Model.User -> Database.Handle -> HadrukiM a) -> HadrukiM a
withUser username f = do
  db <- asks hDatabase
  user <- liftIO $ Model.findUserByUsername db username
  f user db

encodeToken :: LBS8.ByteString -> T.Text
encodeToken = LT.toStrict . LEnc8.decodeUtf8

asLazyBS :: T.Text -> LBS.ByteString
asLazyBS = LEnc8.encodeUtf8 . LT.fromStrict