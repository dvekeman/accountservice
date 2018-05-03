module Hadruki.ApiUtil where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Servant.Server

unauthorized :: ServantErr
unauthorized = err401 { errBody = "Your credentials are invalid." }

failedToGenerateJWT :: ServantErr
failedToGenerateJWT = err500 { errBody = "Failed to generate JWT." }

failedTo :: LBS.ByteString -> ServantErr
failedTo msg = err500 { errBody = msg }
