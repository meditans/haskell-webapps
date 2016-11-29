{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies                  #-}

module Frontend.Roles.Store where

import Control.DeepSeq
import GHC.Generics

import ClassyPrelude
import Common.API
import Common.Roles
import React.Flux
import React.Flux.Addons.Servant


import Data.Proxy

data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String

data RolesStore = RolesStore { roles :: Roles
                             , retrievingStatus   :: RequestStatus
                             }

data RolesStoreAction = InitRolesRequest
                      | InitRolesResponse (Either (Int,String) Roles)
                     deriving (Show, Generic, NFData)

cfg :: ApiRequestConfig Api
cfg = ApiRequestConfig "http://localhost:8081" NoTimeout

instance StoreData RolesStore where
  type StoreAction RolesStore = RolesStoreAction

  transform InitRolesRequest us = do
    putStrLn "Retrieving roles"
    request cfg (Proxy :: Proxy RolesEndpoint) $
      \r -> return . dispatchLogin $ InitRolesResponse r
    return $ us { retrievingStatus = PendingRequest }

  transform (InitRolesResponse (Left (_errCode, err))) us = do
    putStrLn $ "I got an error with roles retrieving" <> tshow _errCode <> tshow err
    return $ us { retrievingStatus = PreviousRequestHadError err }

  transform (InitRolesResponse (Right r)) us = do
    putStrLn $ "Here are the roles: " <> tshow r
    return $ us { retrievingStatus = NoPendingRequest
                , roles = r }

rolesStore :: ReactStore RolesStore
rolesStore = mkStore $ RolesStore (Roles $ mempty) NoPendingRequest

dispatchLogin :: RolesStoreAction -> [SomeStoreAction]
dispatchLogin a = [SomeStoreAction rolesStore a]
