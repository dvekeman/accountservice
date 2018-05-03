{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hadruki.AccountApi where

import           Crypto.JOSE.JWK                           ( JWK )
import           Data.Aeson
import qualified Data.Text as T 
import           Data.Text (Text)
import           Servant
import           Servant.API
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan       ()
import           Servant.HTML.Lucid

import           GHC.Generics

import           Hadruki.Monad
import           Hadruki.Internal.ChangePasswordRequest
import           Hadruki.Internal.SignupRequest
import           Hadruki.Internal.SignupResponse
import           Hadruki.Internal.LoginRequest
import           Hadruki.Internal.LoginResponse
import           Hadruki.Internal.LoginToken

type Api auths = "account" :> 
        (
             "signup" :> ReqBody '[JSON] SignupRequest 
                      :> Post '[JSON] SignupResponse
        :<|> "login"  :> ReqBody '[JSON] LoginRequest
                      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
        :<|> Header "Host" String :> "verify" :> QueryParam "username" Text :>
                         QueryParam "uid" Text :>
                         Verb 'GET 302 '[HTML] ( Headers 
                          '[ Header "Location" Text ] NoContent )
        :<|> Auth auths LoginToken :> "change-password" 
                      :> ReqBody '[JSON] ChangePasswordRequest
                      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
        )
api :: Proxy (Api '[JWT])
api = Proxy
