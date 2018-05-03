{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-} -- For the HasForeign instance
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Hadruki.Web
    ( module Hadruki.Monad
    , Config(..)
    , Handle(..)
    , withHandle
    , Hadruki.Web.run
    , apiServer
    , server
    ) where

import           Crypto.JOSE.JWK                           ( JWK )
import           Control.Applicative                       ( (<|>), empty )
import           Control.Monad.Reader                      ( ReaderT, asks
                                                           , runReaderT )
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Data.Aeson                                ( (.:?), withObject )
import qualified Data.Aeson                                as Aeson
import           Data.Maybe                                ( fromJust
                                                           , fromMaybe )
import           Data.Monoid                               ( (<>) )
import qualified Data.Text                                 as T
import           Data.Text                                 ( Text )
import qualified Data.Text.Extended                        as T
import qualified Data.Text.Lazy                            as LT
import qualified Data.Text.Encoding                        as Enc8
import qualified Data.Text.Lazy.Encoding                   as LEnc8
import qualified Network.HTTP.Types.Header                 as HTTP
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS               ( runTLS
                                                           , tlsSettings )
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan       ()
import           Servant.Foreign.Internal
import           System.Environment       (lookupEnv)
import           System.IO                                 as IO ( hPutStrLn, stderr )

import           Hadruki.Monad
import           Hadruki.AccountApi
import           Hadruki.AccountApiServer
import qualified Hadruki.Database                          as Database
import qualified Hadruki.Model                             as Model
import qualified Hadruki.Logger                            as Logger
import           Hadruki.ApiUtil

import           Debug.Trace

--- Workaround for missing instances in servant-auth ---
instance forall lang ftype api a.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype T.Text
    )
  => HasForeign lang ftype (Auth '[JWT] a :> api) where
  type Foreign ftype (Auth '[JWT] a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "Authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy T.Text)
        }
--- ---

run :: Handle -> IO ()
run h = do
  port <- readport h
  jwk  <- readjwk  h
  IO.hPutStrLn IO.stderr $
      "Running on port " ++ show port 
  Model.createTables (hDatabase h)
  let settings = setPort port defaultSettings
  runSettings settings (app h jwk)

readport :: Handle -> IO Int
readport h = do 
  envPort <- lookupEnv "PORT"
  case envPort of 
    Just port -> return $ read port
    Nothing   -> return $ fromMaybe 8000 $ cPort $ hConfig h

readjwk :: Handle -> IO JWK
readjwk h = generateKey

app :: Handle -> JWK -> Application
app h jwk = 
      cors (const $ Just policy)
    -- $ provideOptions withAssets
    $ serveWithContext api cfg (server h defaultCookieSettings jwtCfg)
  where
  jwtCfg = defaultJWTSettings jwk
  cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
  policy = simpleCorsResourcePolicy
             { corsRequestHeaders = [ "content-type", "authorization" ] }

readerToHandler' :: forall a. Handle -> HadrukiM a -> Handler a
readerToHandler' h r = runReaderT r h

readerToHandler :: Handle -> HadrukiM :~> Handler
readerToHandler h = NT (readerToHandler' h)

server :: Handle -> CookieSettings -> JWTSettings -> Server (Api auths)
server h cs jwts = enter (readerToHandler h) (apiServer cs jwts)
