{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TypeApplications                                       #-}

module Main where

import Common.API
import Common.Permission
import Common.RoleAttributes
import Common.Roles
import Common.User
import Common.UserLogin

import           ClassyPrelude               hiding (Handler, head, span)
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Servant

data Config = Config { rolesTVar :: TVar Roles }

server :: Config -> Server Api
server config
       = authenticate
    :<|> enter (runReaderTNat config) removeUser
    :<|> enter (runReaderTNat config) addRole
    :<|> enter (runReaderTNat config) showRoles
    :<|> serveAssets
  where
    serveAssets = serveDirectory "../frontend/"

authenticate :: (MonadIO m) => UserLogin -> m Text
authenticate u
  | correctInfo = liftIO (threadDelay 1000000) >> return "Authenticated"
  | userPresent = liftIO (threadDelay 1000000) >> return "Wrong password"
  | otherwise   = liftIO (threadDelay 1000000) >> return "Not Authenticated"
  where
    users = M.fromList [ ("user1@gmail.com", "pass1")
                       , ("user2@gmail.com", "pass2")
                       , ("user3@gmail.com", "pass3")
                       ]
    correctInfo = M.lookup (userLoginMail u) users == Just (userLoginPassword u)
    userPresent = userLoginMail u `elem` M.keys users

removeUser :: (MonadReader Config m, MonadIO m) => RoleName -> User -> m NoContent
removeUser rolename user = do
  Config { rolesTVar = roles } <- ask
  liftIO $ atomically $ modifyTVar' roles
    (_Wrapped' . at rolename . _Just . roleAssociatedUsers . contains user .~ False)
  return NoContent

addRole :: (MonadReader Config m, MonadIO m) => RoleName -> RoleAttributes -> m NoContent
addRole rolename roleAttributes = do
  Config { rolesTVar = roles } <- ask
  liftIO $ atomically $ modifyTVar' roles
    (_Wrapped' . at rolename .~ Just roleAttributes)
  return NoContent

showRoles :: (MonadReader Config m, MonadIO m) => m Roles
showRoles = do
  Config { rolesTVar = roles } <- ask
  liftIO $ readTVarIO roles

main :: IO ()
main = do
  state <- atomically $ newTVar exRoles
  let config = Config state
  run 8081 (gzip gzipSettings $ serve (Proxy @Api) (server config))
  where
    gzipSettings = def { gzipFiles = GzipCompress }

--------------------------------------------------------------------------------
---- Example roles to be served by the server:
--------------------------------------------------------------------------------

exRoles = Roles $ accountAdministrator <> productAdministrator <> productEditor

allPermissions :: [Permission]
allPermissions = concat [ map PP [minBound .. maxBound]
                        , map OP [minBound .. maxBound]
                        , map UP [minBound .. maxBound]
                        ]

accountAdministrator = singletonMap "AccountAdministrator" (RoleAttributes roles users)
  where
    roles = setFromList allPermissions
    users = setFromList [ User "admin@mydomain.com"
                        , User "otheradmin@mydomain.com"
                        , User "yetanotheradmin@mydomain.com"
                        ]

productAdministrator = singletonMap "Product administrator" (RoleAttributes roles users)
  where
    roles = setFromList $ map PP [minBound .. maxBound]
    users = setFromList [ User "user1@mydomain.com"
                        , User "user2@mydomain.com"
                        , User "user3@mydomain.com"
                        ]

productEditor = singletonMap "Product editor" (RoleAttributes roles users)
  where
    roles = setFromList $ map PP [ViewAllProductDetails, EditProdTextualContent, EditProdPhotos]
    users = setFromList [ User "user4@mydomain.com"
                        , User "user5@mydomain.com"
                        , User "user6@mydomain.com"
                        , User "user7@mydomain.com"
                        , User "user8@mydomain.com"
                        , User "user9@mydomain.com"
                        , User "user10@mydomain.com"
                        , User "user11@mydomain.com"
                        ]
