{-# LANGUAGE OverloadedStrings, TypeFamilies, NoImplicitPrelude, BangPatterns #-}

module Frontend.EditRole.App where

import Frontend.EditRole.Store

import Frontend.Roles.App (addBars)
import Frontend.Auth.Components

import ClassyPrelude
import React.Flux
import Types
import qualified TabbedApps

import Common.Roles
import Common.RoleAttributes
import Common.User
import Common.Permission

import Control.Lens hiding (view)

editRoleApp :: App TabbedApps.ParentRouter
editRoleApp = App "editRoleApp"
              editRoleStore
              (\_ _ -> view editRoleView () mempty)
              InitRoleRequest
              Nothing

editRoleView :: ReactView ()
editRoleView = defineControllerView "editRoles" editRoleStore (\u _ -> editRoleView' u)

editRoleView' :: EditRoleStore -> ReactElementM ViewEventHandler ()
editRoleView' store = addBars $ do
  div_ ["className"$="col-md-9"] $ do
    topErrors store
    roleNameEdit store
    mainForm store
    editUsers store
    saveButton

topErrors :: EditRoleStore -> ReactElementM ViewEventHandler ()
topErrors st =
  when (st ^. emptyPermissionError) $
    div_ ["className"$="alert alert-danger", "role"$="alert"] $
      span_ $ do
        "Please fix the errors highlighted in "; strong_ "red"; " below:"
        ul_ $ do
            li_ "You must select at least one permission for this role"

roleNameEdit :: EditRoleStore -> ReactElementM ViewEventHandler ()
roleNameEdit st = do
        div_ (["className"$="form-group"] ++ errorMessage (st ^. roleNameError)) --
          $ do
            label_ ["className"$="control-label"] "Role name"
            textInput_ $ TextInputArgs
              { tiaId = Just "passwordField"
              , tiaClass = "form-control"
              , tiaPlaceholder = "User mail"
              , tiaType = "text"
              , tiaOnSave = dispatchEditRole . ModifyCurrentRoleName
              , tiaValue = Nothing
              }
            when (isJust $ st ^. roleNameError) $
              span_ ["className"$="help-block"] "Role name can't be blank"

errorMessage p = if isJust p then ["className"$="has-error"] else []

mainForm :: EditRoleStore -> ReactElementM ViewEventHandler ()
mainForm st = do
  div_ ["className"$="form-group"] $ do
      label_ ["className"$="control-label"] "Permissions"
      div_ ["className"$="row"] $ do
          div_ ["className"$="col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12"] $ do
              div_ ["className"$="checkbox permission-group-heading"] $
                  label_ ["className"$="control-label"] $ do
                      input_ [ "type"$="checkbox"
                             , onClick $ \_ _ -> concatMap (dispatchEditRole . Flip) allProductPermissions]
                      strong_ "Product"
              div_ ["className"$="checkbox"] $ forM_ allProductPermissions $ \p ->
                  label_ ["className"$="control-label"] $ do
                      input_ $ [ "type"$="checkbox"
                             , onChange $ \_ -> dispatchEditRole (Flip p)]
                            ++ checkedIf (st ^. roleAttrs . rolePermission . contains p)
                      elemText $ toUserLabel p
          div_ ["className"$="col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12"] $ do
              div_ ["className"$="checkbox permission-group-heading"] $
                  label_ ["className"$="control-label"] $ do
                      input_ ["type"$="checkbox"]
                      strong_ "Orders"
              div_ ["className"$="checkbox"] $ forM_ allOrderPermissions $ \p ->
                  label_ ["className"$="control-label"] $ do
                      input_ $ [ "type"$="checkbox"
                               , onClick $ \_ _ -> dispatchEditRole (Flip p)]
                            ++ checkedIf (st ^. roleAttrs . rolePermission . contains p)
                      elemText $ toUserLabel p
          div_ ["className"$="col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12"] $ do
              div_ ["className"$="checkbox permission-group-heading"] $
                  label_ ["className"$="control-label"] $ do
                      input_ ["type"$="checkbox"]
                      strong_ "Users"
              div_ ["className"$="checkbox"] $ forM_ allUserPermissions $ \p ->
                  label_ ["className"$="control-label"] $ do
                      input_ $ ["type"$="checkbox"
                             , onChange $ \_ -> dispatchEditRole (Flip p)]
                            ++ checkedIf (st ^. roleAttrs . rolePermission . contains p)
                      elemText $ toUserLabel p

jsCond :: IsString t => Bool -> t
jsCond p = if p then "true" else "false"

checkedIf :: Bool -> [PropertyOrHandler handler]
checkedIf p = if p then ["checked"$="true"] else []

editUsers :: EditRoleStore -> ReactElementM ViewEventHandler ()
editUsers st = do
  div_ (if (st^.addUserError)
        then ["className"$="form-group has-error"]
        else ["className"$="form-group"]) $ do
      label_ ["className"$="control-label"] "Users with this role"
      ul_ $ forM_ (st ^. roleAttrs . roleAssociatedUsers) $ \u@(User t) -> do
          li_ $ do elemText t
                   a_ ["href"$="#"
                      , onClick $ \ _ _ -> dispatchEditRole $ DeleteUser u
                      ] " revoke"
      div_ ["className"$="row"] $
          div_ ["className"$="col-lg-6 col-md-8 col-sm-8 col-xs-12"] $
              div_ ["className"$="input-group"] $ do
                  div_ ["className"$="input-group-addon"] $ span_ "Add another user"
                  textInput_ $ TextInputArgs
                    { tiaId = Just "passwordField"
                    , tiaClass = "form-control"
                    , tiaPlaceholder = "User mail"
                    , tiaType = "text"
                    , tiaOnSave = dispatchEditRole . ModifyCurrentUser
                    , tiaValue = Nothing
                    }
                  div_ ["className"$="input-group-btn"] $
                      button_ [ "className"$="btn btn-default", "type"$="button"
                              , onClick $ \_ _ -> dispatchEditRole $ AddUser] "Add"
      when (st^.addUserError) $
        span_ ["className"$="help-block"] "That doesn't look like a valid user. Does the user exist?"

saveButton :: ReactElementM ViewEventHandler ()
saveButton = do
  div_ ["className"$="form-group"] $ do
    button_ ["className"$="btn btn-primary", "type"$="submit"
            , onClick $ \_ _ -> dispatchEditRole Save] "Save"
    a_ ["href"$="#", "class"$="cancel text-danger"] " cancel"
