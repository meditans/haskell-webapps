{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

{-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Frontend.Auth.Store

import React.Flux
import React.Flux.Ajax

import Frontend.Auth.Components

main :: IO ()
main = do
  initAjax
  reactRender "frontend" frontendApp ()

frontendApp :: ReactView ()
frontendApp = defineControllerView "frontend" userStore (\u _ -> frontendForm u)

frontendForm :: UserStore -> ReactElementM ViewEventHandler ()
frontendForm u =
  div_ ["className"$="login-clean"] $ do
    form_ $ do
      h2_ ["className"$="sr-only"] $ "Login Form"
      div_ ["className"$="illustration"] $
        i_ ["className"$="icon ion-ios-navigate"] mempty
      div_ ["className"$="form-group"] $
        textInput_ $ TextInputArgs
          { tiaId = Just "mailField"
          , tiaClass = "form-control"
          , tiaPlaceholder = "Email"
          , tiaType = "email"
          , tiaOnSave = dispatchLogin . UpdateUser
          , tiaValue = Nothing
          }
      div_ ["className"$="form-group"] $
        textInput_ $ TextInputArgs
          { tiaId = Just "passwordField"
          , tiaClass = "form-control"
          , tiaPlaceholder = "Password"
          , tiaType = "password"
          , tiaOnSave = dispatchLogin . UpdatePassword
          , tiaValue = Nothing
          }
      div_ ["className"$="form-group"] $
        a_ ["className"$="btn btn-primary btn-block", "role"$="button"
           , onClick (\_ _ -> dispatchLogin Auth)] "Log in"
      a_ ["href"$="#", "className"$="forgot"] $ elemText "Forgot your email or password?"
      h2_ (elemText $ message u)
