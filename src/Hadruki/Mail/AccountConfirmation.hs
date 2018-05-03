{-# LANGUAGE QuasiQuotes                #-} -- for shamlet 
{-# LANGUAGE OverloadedStrings          #-} 
module Hadruki.Mail.AccountConfirmation 
  ( sendConfirmationEmail 
  ) where 

import           Control.Monad.IO.Class
import           Control.Monad.Reader         (asks)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc8
import qualified Data.Text.Lazy.Encoding as LEnc8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.Mail.Mime.SES
import           Network.Mail.Mime
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet (shamlet)

import           Hadruki.Monad
import           Hadruki.Aws             as Aws

sendConfirmationEmail :: T.Text -> T.Text -> T.Text -> HadrukiM ()
sendConfirmationEmail username email verurl = do
  awsConfig <- Aws.hConfig <$> asks hAws
  sendAwsConfirmationEmail awsConfig username email verurl 
  
sendAwsConfirmationEmail :: Aws.Config -> T.Text -> T.Text -> T.Text -> HadrukiM ()
sendAwsConfirmationEmail Aws.Config{..} username email verurl = do 
  let recipients = [ email ]
      senderName = "Hadruki.io"
      sender = "dieter.vekeman@tinkhaven.com"
      ses = SES 
              ( Enc8.encodeUtf8 sender )                                      -- ^ From
              ( map Enc8.encodeUtf8 recipients )                              -- ^ To
              ( Enc8.encodeUtf8 accessKey )                                   -- ^ AWS Access Key 
              ( Enc8.encodeUtf8 secretKey )                                   -- ^ AWS Secret Key
--               Nothing                                                         -- ^ Session Token (mime-ses-client 0.4.x.y)
              euWest1                                                         -- ^ Region
      mail = Mail 
              ( Address (Just senderName) sender )                            -- ^ Mail From
              ( map (Address Nothing) recipients )                            -- ^ Mail To
              []                                                              -- ^ Mail CC
              []                                                              -- ^ Mail BCC
              [("Subject", "Verify your email address")]                      -- ^ Mail Headers
              [ [ Part 
                    "text/html; charset=utf-8"                                -- ^ Part type
                    None                                                      -- ^ Part encoding
                    Nothing                                                   -- ^ Part filename
                    []                                                        -- ^ Part headers
                    ( renderHtml 
                        [shamlet|
                          <p>Please confirm your email address by clicking on the link below.
                          <p>
                              <a href=#{verurl}>#{verurl}
                          <p>Thank you
                        |] 
                    )                          -- ^ Part content 
                ] 
              ]
  manager <- liftIO $ newManager tlsManagerSettings
  logInfo $ "Sending confirmation email to " ++ T.unpack email
  logDebug$ "using settings " ++ show ses
  logDebug$ "mail: " ++ show mail
  renderSendMailSES manager ses mail

