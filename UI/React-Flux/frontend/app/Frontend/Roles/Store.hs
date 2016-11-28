{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies                  #-}

module Frontend.Roles.Store where

import Control.DeepSeq
import GHC.Generics

import ClassyPrelude
import Common.API
import Common.UserLogin
import React.Flux
import React.Flux.Addons.Servant

import Data.Proxy

data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String

data RolesStore = RolesStore

data RolesStoreAction = RolesStoreAction
                     deriving (Show, Generic, NFData)

cfg :: ApiRequestConfig Api
cfg = ApiRequestConfig "http://localhost:8081" NoTimeout

instance StoreData RolesStore where
  type StoreAction RolesStore = RolesStoreAction

  transform RolesStoreAction us = return us

rolesStore :: ReactStore RolesStore
rolesStore = mkStore RolesStore

dispatchLogin :: RolesStoreAction -> [SomeStoreAction]
dispatchLogin a = [SomeStoreAction rolesStore a]
