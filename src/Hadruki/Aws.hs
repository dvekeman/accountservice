module Hadruki.Aws
  ( Handle (..)
  , Config (..)
  , withHandle
  ) where

import qualified Data.Aeson               as A
import           Data.Monoid              ((<>))
import qualified Data.Text as T

data Config = Config
    { accessKey :: T.Text
    , secretKey :: T.Text
    } deriving (Show)

instance Monoid Config where
    mempty                        = Config mempty mempty
    mappend l r = Config
        { accessKey     = accessKey     l <> accessKey     r
        , secretKey     = secretKey     l <> secretKey     r
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Config" $ \o ->
        Config <$> o A..: "access_key"
               <*> o A..: "secret_key"

newtype Handle = Handle
    { hConfig :: Config
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = do
    let handle = Handle 
                  { hConfig = config
                  }
    f handle