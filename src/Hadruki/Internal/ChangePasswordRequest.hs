module Hadruki.Internal.ChangePasswordRequest where

import           Data.Aeson
import qualified Data.Text.Extended as T
import           GHC.Generics

data ChangePasswordRequest = ChangePasswordRequest
   { username :: T.Text
   , oldpassword :: T.Text
   , newpassword :: T.Text
   } deriving (Show, Eq, Generic)

instance FromJSON ChangePasswordRequest