{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards                #-}

module Types where

import ClassyPrelude
import Data.Typeable (Typeable)
import React.Flux

type AppName = Text
type AppView = ReactElementM ViewEventHandler
type AppRouter = [Text] -> IO ()

data App props = forall state. StoreData state =>
           App { appName       :: AppName
               , appState      :: ReactStore state
               , appView       :: Typeable props => state -> props -> AppView ()
               , appInitAction :: StoreAction state
               , appRouter     :: Maybe AppRouter
               } deriving Typeable

initApp :: Typeable props => App props -> IO (ReactView props)
initApp App{..} = do
  let view' = defineControllerView (unpack appName) appState appView
  alterStore appState appInitAction
  return view'
