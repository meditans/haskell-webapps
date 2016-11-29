{-# LANGUAGE DeriveGeneric, TemplateHaskell, DeriveAnyClass #-}

module Common.RoleAttributes where

import Common.Prelude
import Common.Permission
import Common.User
import Control.DeepSeq

data RoleAttributes = RoleAttributes
  { _rolePermission      :: Set Permission
  , _roleAssociatedUsers :: Set User
  } deriving (Show, Eq, Ord, Generic, NFData)

emptyRoleAttributes :: RoleAttributes
emptyRoleAttributes = RoleAttributes mempty mempty

makeLenses ''RoleAttributes

instance ToJSON RoleAttributes
instance FromJSON RoleAttributes
