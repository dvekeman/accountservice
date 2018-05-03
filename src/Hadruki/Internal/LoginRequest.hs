module Hadruki.Internal.LoginRequest where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data LoginRequest = LoginRequest { username :: T.Text
                                 , password :: T.Text
                                 }
    deriving (Show, Eq, Generic)

instance FromJSON LoginRequest