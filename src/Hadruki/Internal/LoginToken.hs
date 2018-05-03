module Hadruki.Internal.LoginToken where

import           Data.Aeson
import qualified Data.Text as T
import           Servant.Auth.Server
import           GHC.Generics

data LoginToken = LoginToken { loginname  :: T.Text
                             , loginemail :: T.Text
                             }
    deriving (Eq, Show, Read, Generic)

instance ToJSON LoginToken

instance ToJWT LoginToken

instance FromJSON LoginToken

instance FromJWT LoginToken
