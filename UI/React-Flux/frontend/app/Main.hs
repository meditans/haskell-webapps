{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Frontend.Auth.App
import Frontend.Roles.App

import React.Flux
import React.Flux.Ajax

import ClassyPrelude
import qualified TabbedApps
import Router
import Types

main :: IO ()
main = do
  initAjax
  let apps = [authApp, rolesApp]
  appViews <- mapM initApp apps
  let tabs = appsToTabs "main tabs" apps appViews
  tabView <- initApp tabs
  case tabs of
    App {appRouter = Just ar} -> initRouter ar
    _ -> return ()
  putStrLn "hey"
  reactRender "frontend" tabView Nothing
  where
    appsToTabs tabsName apps appViews =
      tabApp tabsName $
      zipWith
        (\a v ->
           TabbedApps.Tab (appName a) (\pr -> view v pr mempty) (appRouter a))
        apps
        appViews

tabApp :: Text -> [TabbedApps.Tab] -> App TabbedApps.ParentRouter
tabApp name tabs =
  let rst = TabbedApps.newStore tabs
  in App name
         rst
         (\st rt -> TabbedApps.view_ rst rt st)
         TabbedApps.TabbedInit
         (Just $ storeRouter rst)

