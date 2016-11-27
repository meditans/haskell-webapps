{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module TabbedApps (TabbedAction(..)
                  , TabbedState(..)
                  , Tab(..)
                  , ParentRouter
                  , dispatch
                  , newStore
                  , view
                  , view_) where

import           React.Flux          hiding (view)
import qualified React.Flux          as RF

-- import           Router
import           Types

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
               , tabRouter :: Maybe AppRouter
               }

data TabbedState = TabbedState { tsFocus :: !Int
                               , tsTabs  :: ![Tab]
                               } deriving Typeable

data TabbedAction = SwitchApp !Int (Maybe [T.Text])
                  | TabbedInit
                  deriving (Show, Typeable, Generic, NFData)

instance WR.PathInfo TabbedAction where
  toPathSegments (SwitchApp aidx apath) =
    "switchapp":WR.toPathSegments aidx ++ fromMaybe [] apath
  toPathSegments TabbedInit = ["tabbedinit"]
  fromPathSegments = SwitchApp <$ WR.segment "switchapp"
                     <*> WR.pToken ("app-num"::String) intParser
                     <*> WR.patternParse subRouteParser
                     <|> TabbedInit <$ WR.segment "tabbedinit"
    where
      intParser v =
        case TR.decimal v of
        Right (aidx, "") -> Just aidx
        _ -> Nothing
      subRouteParser apath =
        Right $ if null apath then Nothing else Just apath

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
      SwitchApp idx tabRoute
        | idx >= 0 && idx <= length tsTabs -> do
            let Tab{..} = tsTabs !! idx
            case (tabRouter, tabRoute) of
              (Just tr, Just rt) ->
                tr rt
              _ ->
                return ()
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
    router cur action =
      actionRoute prouter $ SwitchApp cur (Just $ childRoutePath action)

tabItem :: ReactView ((ParentRouter, Int), (Int, AppName))
tabItem =
  defineView "tab-item" $ \((prouter, cur), (aidx, aname)) ->
  span_ ["style" @= A.object ["color" A..= ("#eee"::String)] | cur == aidx] $
  if cur == aidx
  then elemText aname
  else a_ ["href" &= actionRoute prouter (SwitchApp aidx Nothing)] $ elemText aname

tabItem_ :: ((ParentRouter, Int), (Int, AppName)) -> ReactElementM eventHandler ()
tabItem_ tab =
  viewWithKey tabItem (fst $ snd tab) tab mempty

view_ :: ReactStore TabbedState -> ParentRouter -> TabbedState -> ReactElementM eventHandler ()
view_ rst pr st =
  RF.view (view rst pr) st mempty

dispatch :: ReactStore TabbedState -> TabbedAction -> [SomeStoreAction]
dispatch rst a = [SomeStoreAction rst a]
