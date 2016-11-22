{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.UserLogin where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API
import React.Flux
import Control.DeepSeq
import Data.Typeable

data User = User
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, Generic, Typeable)

instance ToJSON User
instance FromJSON User

type MockApi = "auth" :> ReqBody '[JSON] User :> Post '[JSON] Text
          :<|> "assets" :> Raw
          :<|> Raw
