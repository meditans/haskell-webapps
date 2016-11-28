{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Frontend.Roles.App where

import Frontend.Roles.Store

import React.Flux
import Types
import qualified TabbedApps

rolesApp :: App TabbedApps.ParentRouter
rolesApp = App "rolesApp"
              rolesStore
              (\_ _ -> view rolesView () mempty)
              RolesStoreAction
              Nothing

rolesView :: ReactView ()
rolesView = defineControllerView "roles" rolesStore (\u _ -> rolesView' u)

-- rolesView' :: RolesStore -> ReactElementM ViewEventHandler ()
-- rolesView' _ =
--   div_ ["className"$="login-clean"] $ do
--     h2_ (elemText $ "hey")

rolesView' :: RolesStore -> ReactElementM ViewEventHandler ()
rolesView' _ = do
  topBar

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

-- rolesView' :: RolesStore -> ReactElementM ViewEventHandler ()
-- rolesView' _ = div_ $
--   div_ ["class_"$="container"] $ do
--     div_ ["class_"$="navbar-header"] $ do
--       a_ ["class_"$="navbar-brand navbar-link", "href"$="#"] $ "Tenant name comes here"
--       button_ "class_"$="navbar-toggle collapsed" "dataAttribute"$="toggle" "collapse" dataAttribute "target" "#navcol-1" $ do
--         span_ ["class_"$="sr-only"] $ "Toggle navigation"
--         span_ ["class_"$="icon-bar"] $ mempty
--         span_ ["class_"$="icon-bar"] $ mempty
--         span_ ["class_"$="icon-bar"] $ mempty
--         --     div_ class_ "collapse navbar-collapse" A.id "navcol-1" $ do
--         --         ul class_ "nav navbar-nav" $ do
--         --             li class_ "active" $ a href "#" $ "Link 1"
--         --             li $ a href "#" $ "Link 2"
--         --             li $ a href "#" $ "Link 3"
--         --         form class_ "navbar-form navbar-left" target "_self" $ div_ class_ "form-group" $ do
--         --             H.label class_ "control-label" for "search-field" $ i class_ "glyphicon glyphicon-search" $ mempty
--         --             input class_ "form-control search-field" type_ "search" name "search" A.id "search-field"
--         -- div_ mempty
--         -- div_ $ div_ class_ "container" $ div_ class_ "row" $ do
--         --     div_ class_ "col-md-3 secton-menu" $ ul class_ "nav nav-pills nav-stacked" $ do
--         --         li class_ "active" $ a href "#" $ "Account Settings"
--         --         li $ a href "#" $ "Products"
--         --         li $ a href "#" $ "Orders"
--         --     div_ class_ "col-md-9" $ do
--         --         ol class_ "breadcrumb" $ do
--         --             li $ a $ span_ "Account settings"
--         --             li $ a $ span_ "Roles"
--         --         button class_ "btn btn-primary pull-right" type_ "button" $ "New role"
--         --         h1 class_ "page-heading" $ "Roles"
--         --         div_ class_ "table-responsive" $ table class_ "table" $ do
--         --             thead $ tr $ do
--         --                 th "Role name"
--         --                 th "Permissions"
--         --                 th "Users"
--         --             tbody $ do
--         --                 tr $ do
--         --                     td $ do
--         --                         "Account administrator"
--         --                         a href "role-edit.html" $ "(edit)"
--         --                     td $ em "All permissions"
--         --                     td $ ul $ do
--         --                         li "admin@mydomain.com"
--         --                         li $ do
--         --                             "otheradmin@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ do
--         --                             "yetanotheradmin@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                 tr $ do
--         --                     td $ do
--         --                         "Product administrator"
--         --                         a href "role-edit.html" $ "(edit)"
--         --                     td $ ul $ do
--         --                         li "View product"
--         --                         li "Edit product textual content"
--         --                         li "Edit product properties"
--         --                         li "Edit product price"
--         --                         li $ a href "#" $ "+ 8 more"
--         --                     td $ ul $ do
--         --                         li $ do
--         --                             "user1@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ do
--         --                             "user2@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ do
--         --                             "user3@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                 tr $ do
--         --                     td $ do
--         --                         "Product editor"
--         --                         a href "role-edit.html" $ "(edit)"
--         --                     td $ ul $ do
--         --                         li "View product"
--         --                         li "Edit product textual content"
--         --                         li "Edit product images"
--         --                     td $ ul $ do
--         --                         li $ do
--         --                             "user4@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ do
--         --                             "user3@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ do
--         --                             "user7@mydomain.com"
--         --                             a href "#" $ "(revoke)"
--         --                         li $ a href "#" $ "+ 5 more"
