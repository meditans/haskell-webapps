{-# LANGUAGE OverloadedStrings, TypeFamilies, NoImplicitPrelude #-}

module Frontend.Roles.App where

import Frontend.Roles.Store

import ClassyPrelude
import React.Flux
import Types
import qualified TabbedApps

import Common.Roles
import Common.RoleAttributes
import Common.User
import Common.Permission

rolesApp :: App TabbedApps.ParentRouter
rolesApp = App "rolesApp"
              rolesStore
              (\_ _ -> view rolesView () mempty)
              InitRolesRequest
              Nothing

rolesView :: ReactView ()
rolesView = defineControllerView "roles" rolesStore (\u _ -> rolesView' u)

rolesView' :: RolesStore -> ReactElementM ViewEventHandler ()
rolesView' store = addBars $ rolesPresentation (roles store)

--------------------------------------------------------------------------------
-- Common components
--------------------------------------------------------------------------------

addBars :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
addBars content = do
  topBar
  div_ $
    div_ ["className"$="container"] $
      div_ ["className"$="row"] $ do
        lateralBar
        content

topBar :: ReactElementM ViewEventHandler ()
topBar =
      div_ $
        nav_ ["className"$="navbar navbar-inverse navigation-clean-search"] $ do
            div_ ["className"$="container"] $ do
                div_ ["className"$="navbar-header"] $ do
                  a_ ["className"$="navbar-brand navbar-link", "href"$="#"] $
                    elemText "Tenant name comes here"
                  button_ ["className"$="navbar-toggle collapsed", "data-toggle"$="collapse", "data-target"$="#navcol-1"] $ do
                    span_ ["className"$="sr-only"] "Toggle navigation"
                    span_ ["className"$="icon-bar"] mempty
                    span_ ["className"$="icon-bar"] mempty
                    span_ ["className"$="icon-bar"] mempty
                div_ ["className"$="collapse navbar-collapse", "id"$="navcol-1"] $ do
                    ul_ ["className"$="nav navbar-nav"] $ do
                        li_ ["className"$="active", "role"$="presentation"] $
                          a_ ["href"$="#"] "Link 1"
                        li_ ["role"$="presentation"] $
                          a_ ["href"$="#"] "Link 2"
                        li_ ["role"$="presentation"] $
                          a_ ["href"$="#"] "Link 3"
                    form_ ["className"$="navbar-form navbar-left", "target"$="_self"] $
                        div_ ["className"$="form-group"] $ do
                            label_ ["className"$="control-label", "for"$="search-field"] $
                              i_ ["className"$="glyphicon glyphicon-search"] mempty
                            input_ [ "className"$="form-control search-field"
                                   , "type"$="search"
                                   , "name"$="search"
                                   , "id"$="search-field"]


lateralBar :: ReactElementM ViewEventHandler ()
lateralBar =
  div_ ["className"$="col-md-3 secton-menu"] $
    ul_ ["className"$="nav nav-pills nav-stacked"] $ do
      li_ ["className"$="active"] $ a_ ["href"$="#"] "Account Settings"
      li_ [] $ a_ ["href"$="#"] "Products"
      li_ [] $ a_ ["href"$="#"] "Orders"

--------------------------------------------------------------------------------
-- Presentation for the roles page
--------------------------------------------------------------------------------

rolesPresentation :: Roles -> ReactElementM ViewEventHandler ()
rolesPresentation rs =
  div_ ["className"$="col-md-9"] $ do
    ol_ ["className"$="breadcrumb"] $ do
        li_ $ a_ $ span_ "Account settings"
        li_ $ a_ $ span_ "Roles"
    button_ ["className"$="btn btn-primary pull-right", "type"$="button"] "New role"
    h1_ ["className"$="page-heading"] "Roles"
    div_ ["className"$="table-responsive"] $
        table_ ["className"$="table"] $ do
            thead_ $
                tr_ $ do
                    th_ "Role name"
                    th_ "Permissions"
                    th_ "Users"
            tbody_ $ mapM_ role (mapToList . unRoles $ rs)

role :: (RoleName, RoleAttributes) -> ReactElementM ViewEventHandler ()
role (roleName, roleAttrs) =
  tr_ $ do
    td_ $ do
        elemText roleName
        a_ ["href"$="role-edit.html"] " (edit)"
    td_ $ ul_ $ forM_ (setToList $ _rolePermission $ roleAttrs) $ \p -> do
        li_ $ do
          elemText $ toUserLabel p
    td_ $ ul_ $ forM_ (setToList $ _roleAssociatedUsers $ roleAttrs) $ \u -> do
        li_ $ do
          elemText $ userMail u
          a_ ["href"$="#"] " (revoke)"
