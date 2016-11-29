{-# LANGUAGE DeriveGeneric, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Common.Roles where

import Control.DeepSeq
import Common.Prelude
import Common.RoleAttributes
import           Data.Map (Map)

type RoleName = Text

newtype Roles = Roles { unRoles :: Map RoleName RoleAttributes } deriving (Show, Eq, Ord, Generic, NFData)

instance Wrapped Roles where
  type Unwrapped Roles = Map RoleName RoleAttributes
  _Wrapped' = iso unRoles Roles

instance ToJSON Roles
instance FromJSON Roles
