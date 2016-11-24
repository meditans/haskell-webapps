{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Common.API where

import Data.Text
import Servant.API

import Common.Roles
import Common.UserLogin
import Common.RoleAttributes
import Common.User

type Api = AuthEndpoint :<|> DeleteUserEndpoint :<|> AddUserEndpoint :<|> RolesEndpoint :<|> Raw

type AuthEndpoint       = "auth" :> ReqBody '[JSON] UserLogin :> Post '[JSON] Text
type DeleteUserEndpoint = "delete" :> Capture "role" RoleName :> Capture "user" User :> Delete '[JSON] NoContent
type AddUserEndpoint    = "add" :> Capture "role" RoleName :> ReqBody '[JSON] RoleAttributes :> Put '[JSON] NoContent
type RolesEndpoint      = "roles" :> Get '[JSON] Roles
