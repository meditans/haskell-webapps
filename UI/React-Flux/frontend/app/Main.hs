{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import React.Flux
import Common.UserLogin
import Data.Monoid

main :: IO ()
main = reactRender "frontend" frontendApp ()

frontendApp :: ReactView ()
frontendApp = defineControllerView "frontend" store $ \_ _ -> do
  div_ frontendForm_

frontendForm_ :: ReactElementM eventHandler ()
frontendForm_ = view frontendForm () mempty

frontendForm :: ReactView ()
frontendForm = defineView "header" $ \() ->
  ul_ $ do li_ (b_ "Hello")
           li_ "World"
           li_ $
             ul_ (li_ "Nested" <> li_ "List")

store :: ReactStore User
store = mkStore $ User "meditans@gmail.com" "password"

instance StoreData User where
  type StoreAction User = ()
  transform () u = return u


