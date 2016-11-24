{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE TypeOperators                                               #-}

module Common.UserLogin where

import Control.DeepSeq
import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics

data UserLogin = UserLogin
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, Generic, Typeable, NFData)

instance ToJSON UserLogin
instance FromJSON UserLogin
