module Hadruki.Internal.SignupRequest where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data SignupRequest = SignupRequest { username :: T.Text
                                   , email    :: T.Text
                                   , password :: T.Text
                                   , app      :: T.Text
                                   }
    deriving (Show, Eq, Generic)

instance FromJSON SignupRequest