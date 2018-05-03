module Hadruki.Internal.LoginResponse where

import           Data.Aeson
import qualified Data.Text               as T
import           Servant.Auth.Server.Internal.JWT
import           GHC.Generics

data LoginResponse = LoginResponse { username :: T.Text
                                   , email    :: T.Text
                                   , token    :: T.Text
                                   }
    deriving (Show, Eq, Generic)

instance ToJSON LoginResponse
instance FromJSON LoginResponse

instance ToJWT LoginResponse
instance FromJWT LoginResponse