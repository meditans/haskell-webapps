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
  { user      :: User
  , reqStatus :: RequestStatus
  , message   :: Text
  }

data UserStoreAction = UpdateUser Text
                     | UpdatePassword Text
                     | Auth
                     | AuthResponse User (Either (Int,String) Text)
                     deriving (Show, Generic, NFData)

cfg :: ApiRequestConfig Api
cfg = ApiRequestConfig "localhost:8081" NoTimeout

instance StoreData UserStore where
  type StoreAction UserStore = UserStoreAction

  transform (UpdateUser t) us = return us {user = upd (user us)}
    where upd (User _ p) = User t p

  transform (UpdatePassword t) us = return us {user = upd (user us)}
    where upd (User u _) = User u t

  transform Auth us = do
    putStrLn $ "I received " <> tshow (user us)
    request cfg (Proxy :: Proxy Auth) (user us) $
      \r -> return . dispatchLogin $ AuthResponse (user us) r
    return $ us { reqStatus = PendingRequest
                , message = tshow (user us) }

  transform (AuthResponse _ (Left (_errCode, err))) us =
    return $ us { reqStatus = PreviousRequestHadError err
                , message = pack err}

  transform (AuthResponse _ (Right t)) us =
    return $ us { reqStatus = NoPendingRequest
                , message = t }

userStore :: ReactStore UserStore
userStore = mkStore $ UserStore (User "" "") NoPendingRequest ""

dispatchLogin :: UserStoreAction -> [SomeStoreAction]
dispatchLogin a = [SomeStoreAction userStore a]
