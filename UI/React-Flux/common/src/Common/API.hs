{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Common.API where

import Common.UserLogin
import Data.Text
import Servant.API

type Auth = "auth" :> ReqBody '[JSON] User :> Post '[JSON] Text

type Api = Auth :<|> Raw
