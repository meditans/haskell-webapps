{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Common.Roles where

import Common.Prelude
import Common.RoleAttributes
import           Data.Map (Map)
import qualified Data.Map as Map

type RoleName = Text

newtype Roles = Roles { unRoles :: Map RoleName RoleAttributes } deriving (Show, Eq, Ord, Generic)

instance Wrapped Roles where
  type Unwrapped Roles = Map RoleName RoleAttributes
  _Wrapped' = iso unRoles Roles

instance ToJSON Roles
instance FromJSON Roles
