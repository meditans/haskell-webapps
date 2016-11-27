{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies                  #-}

module Frontend.Auth.Store where

import Control.DeepSeq
import GHC.Generics

import ClassyPrelude
import Common.API
import Common.UserLogin
import React.Flux
import React.Flux.Addons.Servant

import Data.Proxy

data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String

data UserStore = UserStore
  { user      :: UserLogin
  , reqStatus :: RequestStatus
  , message   :: Text
  }

data UserStoreAction = UpdateUser Text
                     | UpdatePassword Text
                     | Auth
                     | AuthResponse (Either (Int,String) Text)
                     deriving (Show, Generic, NFData)

cfg :: ApiRequestConfig Api
cfg = ApiRequestConfig "http://localhost:8081" NoTimeout

instance StoreData UserStore where
  type StoreAction UserStore = UserStoreAction

  transform (UpdateUser t) us = return us {user = upd (user us)}
    where upd (UserLogin _ p) = UserLogin t p

  transform (UpdatePassword t) us = return us {user = upd (user us)}
    where upd (UserLogin u _) = UserLogin u t

  transform Auth us = do
    putStrLn $ "I received " <> tshow (user us)
    request cfg (Proxy :: Proxy AuthEndpoint) (user us) $
      \r -> return . dispatchLogin $ AuthResponse r
    return $ us { reqStatus = PendingRequest }

  transform (AuthResponse (Left (_errCode, err))) us =
    return $ us { reqStatus = PreviousRequestHadError err
                , message = pack err }

  transform (AuthResponse (Right t)) us =
    return $ us { reqStatus = NoPendingRequest
                , message = t }

userStore :: ReactStore UserStore
userStore = mkStore $ UserStore (UserLogin "" "") NoPendingRequest ""

dispatchLogin :: UserStoreAction -> [SomeStoreAction]
dispatchLogin a = [SomeStoreAction userStore a]
