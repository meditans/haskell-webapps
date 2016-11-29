{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes   #-}
{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies      #-}

module TabbedApps2 ( TabbedAction(..)
                   , TabbedState(..)
                   , Tab(..)
                   , ParentRouter
                   , dispatch
                   , newStore
                   , view
                   , view_) where

import           React.Flux hiding (view)
import qualified React.Flux as RF

import React.Flux.Internal (toJSString)

import Router
import Types

import           Control.Applicative ((<|>))
import           Control.DeepSeq
import qualified Data.Aeson          as A
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import qualified Web.Routes          as WR

type ParentRouter = Maybe ([T.Text] -> T.Text)

data Tab = Tab { tabName   :: AppName
               , tabView   :: ParentRouter -> AppView ()
               }

data TabbedState = TabbedState { tsFocus :: !Int
                               , tsTabs  :: ![Tab]
                               } deriving Typeable

data TabbedAction = SwitchApp !Int
                  | TabbedInit
                  deriving (Show, Typeable, Generic, NFData)

instance WR.PathInfo TabbedAction where
  toPathSegments (SwitchApp aidx) =
    "switchapp":WR.toPathSegments aidx
  toPathSegments TabbedInit = ["tabbedinit"]
  fromPathSegments = SwitchApp <$ WR.segment "switchapp"
                     <*> WR.pToken ("app-num"::String) intParser
                     <|> TabbedInit <$ WR.segment "tabbedinit"
    where
      intParser v =
        case TR.decimal v of
        Right (aidx, "") -> Just aidx
        _                -> Nothing

instance Show TabbedState where
  showsPrec prec TabbedState{..} =
    showsPrec prec ("TabbedState" :: String, tsFocus, map tabName tsTabs)

instance StoreData TabbedState where
  type StoreAction TabbedState = TabbedAction
  transform action st@TabbedState{..} = do
    putStrLn $ "Action: " ++ show action
    case action of
      TabbedInit ->
        return st
      SwitchApp idx
        | idx >= 0 && idx <= length tsTabs -> do
            return $ st{tsFocus = idx}
        | otherwise ->
          error $ "Application index is out of range " ++ show idx

newStore :: [Tab] -> ReactStore TabbedState
newStore tabs = mkStore $ TabbedState 0 tabs

view :: ReactStore TabbedState -> ParentRouter -> ReactView TabbedState
view _ prouter = defineView "tabbed" $ \TabbedState{..} ->
  div_ $ do
    div_ ["className" $= "tabbed-app-picker"] $
      mapM_ (tabItem_ . ((prouter,tsFocus),)) $ zip [0..] $ map tabName tsTabs
    div_ ["className" $= "tabbed-internal-app"] $
      if tsFocus < length tsTabs
       then tabView (tsTabs !! tsFocus) (Just $ router tsFocus)
      else mempty
  where
    router cur _ = actionRoute prouter $ SwitchApp cur

tabItem :: ReactView ((ParentRouter, Int), (Int, AppName))
tabItem =
  defineView "tab-item" $ \((prouter, cur), (aidx, aname)) ->
  span_ ["style" @= A.object ["color" A..= ("#eee"::String)] | cur == aidx] $
  if cur == aidx
  then elemText aname
  else a_ ["href" &= actionRoute prouter (SwitchApp aidx)] $ elemText aname

tabItem_ :: ((ParentRouter, Int), (Int, AppName)) -> ReactElementM eventHandler ()
tabItem_ tab =
  viewWithKey tabItem (fst $ snd tab) tab mempty

view_ :: ReactStore TabbedState -> ParentRouter -> TabbedState -> ReactElementM eventHandler ()
view_ rst pr st =
  RF.view (view rst pr) st mempty

dispatch :: ReactStore TabbedState -> TabbedAction -> [SomeStoreAction]
dispatch rst a = [SomeStoreAction rst a]
