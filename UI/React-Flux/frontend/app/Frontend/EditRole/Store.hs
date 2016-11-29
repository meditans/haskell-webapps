{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, BangPatterns #-}

module Frontend.EditRole.Store where

import Control.DeepSeq
import GHC.Generics hiding (to)

import ClassyPrelude
import Common.API
import Common.Roles
import Common.RoleAttributes
import Common.Permission
import Common.User

import React.Flux
import React.Flux.Addons.Servant

import Data.Proxy
import Control.Lens

data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String
                   deriving (Show)

data EditRoleStore = EditRoleStore { _roleName  :: !RoleName
                                   , _roleNameError :: Maybe Text
                                   , _roleAttrs :: !RoleAttributes
                                   , _retrievingStatus :: !RequestStatus
                                   , _currentUser :: Text
                                   , _currentRoleName :: Text
                                   , _addUserError :: Bool
                                   , _emptyPermissionError :: Bool
                                   } deriving (Show)

makeLenses ''EditRoleStore

data EditRoleAction = InitRoleRequest
                    | InitRoleResponse (Either (Int,String) Roles)
                    | Flip Permission
                    | AddUser
                    | DeleteUser User
                    | ModifyCurrentUser Text
                    | ModifyCurrentRoleName Text
                    | Save
                    deriving (Show, Generic, NFData)

cfg :: ApiRequestConfig Api
cfg = ApiRequestConfig "http://localhost:8081" NoTimeout

instance StoreData EditRoleStore where
  type StoreAction EditRoleStore = EditRoleAction

  transform InitRoleRequest us = do
    putStrLn "Retrieving roles"
    request cfg (Proxy :: Proxy RolesEndpoint) $
      \r -> return . dispatchEditRole $ InitRoleResponse r
    return $ retrievingStatus .~ PendingRequest $ us

  transform (InitRoleResponse (Left (_errCode, err))) us = do
    putStrLn $ "I got an error with roles retrieving" <> tshow _errCode <> tshow err
    return $ retrievingStatus .~ PreviousRequestHadError err $ us

  transform (InitRoleResponse (Right r)) us = do
    putStrLn $ "Here are the roles: " <> tshow r
    let attrs = maybe (RoleAttributes mempty mempty) id $ lookup (us ^. roleName) (unRoles r)
    return $ us & roleAttrs .~ attrs
                & retrievingStatus .~ NoPendingRequest

  transform (Flip p) us = do
    putStrLn $ "The old property " <> tshow p <> " was " <> tshow (us ^. roleAttrs . rolePermission . contains p)
    return $ (roleAttrs . rolePermission . contains p %~ not) $ us

  transform AddUser us = do
    putStrLn $ "I'm adding " <> (us ^. currentUser) <> " to the users"
    let thereIsError = null (us ^. currentUser)
        addError = addUserError .~ thereIsError
        addUser = (roleAttrs . roleAssociatedUsers . contains (User (us ^. currentUser)) .~ True)
    putStrLn $ "thereIsError: " <> tshow thereIsError
    return $ addError . (if thereIsError then id else addUser) $ us

  transform (DeleteUser u) us = do
    putStrLn $ "I'm deleting" <> tshow u <> " from the users"
    return $ (roleAttrs . roleAssociatedUsers . contains u .~ False) $ us

  transform (ModifyCurrentUser t) us = do
    putStrLn $ "I called ModifyCurrentUser with text " <> t
    return $ (currentUser .~ t) $ us

  transform (ModifyCurrentRoleName t) us = do
    putStrLn $ "I called ModifyCurrentRoleName with text " <> t
    putStrLn $ "Current error was " <> tshow (us ^. roleNameError)
    putStrLn $ tshow $ null t
    let errorMod = (roleNameError .~ if null (t :: Text) then Just "Role name can't be blank" else Nothing)
    let us' = errorMod . (currentRoleName .~ t) $ us
    putStrLn $ "The entire new state is: " <> tshow us'
    return us'

  transform (Save) us = do
    let addError = emptyPermissionError .~ (us ^. roleAttrs . rolePermission . to null)
    return $ addError us

editRoleStore :: ReactStore EditRoleStore
editRoleStore = mkStore $
 EditRoleStore { _roleName  = "AccountAdministrator"
               , _roleNameError = Nothing
               , _roleAttrs = RoleAttributes mempty mempty
               , _retrievingStatus = NoPendingRequest
               , _currentUser = ""
               , _currentRoleName = ""
               , _addUserError = False
               , _emptyPermissionError = False
               }

dispatchEditRole :: EditRoleAction -> [SomeStoreAction]
dispatchEditRole a = [SomeStoreAction editRoleStore a]
