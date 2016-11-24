{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE TypeOperators                                               #-}

module Common.UserLogin where

import Control.DeepSeq
import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics

data User = User
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, Generic, Typeable, NFData)

instance ToJSON User
instance FromJSON User
