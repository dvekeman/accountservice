module Hadruki.Internal.SignupResponse where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data SignupResponse = SignupResponse { username :: T.Text
                                     , email    :: T.Text
                                     }
    deriving (Show, Eq, Generic)

instance ToJSON SignupResponse