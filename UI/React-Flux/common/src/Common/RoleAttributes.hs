{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.RoleAttributes where

import Common.Prelude
import Common.Permission
import Common.User

data RoleAttributes = RoleAttributes
  { _rolePermission      :: Set Permission
  , _roleAssociatedUsers :: Set User
  } deriving (Show, Eq, Ord, Generic)

emptyRoleAttributes :: RoleAttributes
emptyRoleAttributes = RoleAttributes mempty mempty

makeLenses ''RoleAttributes

instance ToJSON RoleAttributes
instance FromJSON RoleAttributes
