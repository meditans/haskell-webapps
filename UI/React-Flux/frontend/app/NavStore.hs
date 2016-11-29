{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, CPP #-}


-- | A store to hold the current page being viewed.
module NavStore where

import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import React.Flux

#ifdef __GHCJS__
import GHCJS.Types (JSVal, JSString)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHCJS.Marshal (fromJSVal)
import Data.JSString (pack)
# else
data JSString
data Callback a = ContinueAsync
data JSVal
pack = undefined
syncCallback1 = undefined
fromJSVal = undefined
#endif

data NavPageId = Page1
               | Page2
               | Page3
    deriving (Show, Eq, Enum, Bounded, Typeable, Generic, NFData)

-- | The page title (used for `document.title`) and the url (used for the history API and displayed
-- in the browser location bar).
pageTitleAndUrl :: NavPageId -> (String, String)
pageTitleAndUrl Page1 = ("Page 1 Title", "/page1")
pageTitleAndUrl Page2 = ("Page 2222", "/pages/page2")
pageTitleAndUrl Page3 = ("Page Three Title", "/page3.html")

allPageIds :: [NavPageId]
allPageIds = [(minBound :: NavPageId) .. ]

data NavAction = ChangePageTo NavPageId -- ^ use to change the page
               | BackToPage NavPageId -- ^ used from the history handler to go back
               | ToggleSideMenu
    deriving (Show, Eq, Typeable, Generic, NFData)

data NavState = NavState
  { sideMenuOpen :: Bool
  , currentPageId :: NavPageId
  } deriving (Show, Eq, Generic, Typeable)

instance StoreData NavState where
    type StoreAction NavState = NavAction
    transform (ChangePageTo p) _ = do
        historyPushState p
        setDocTitle p
        return NavState { currentPageId = p, sideMenuOpen = False }
    transform (BackToPage p) _ = do
        setDocTitle p
        return NavState { currentPageId = p, sideMenuOpen = False }
    transform ToggleSideMenu s =
        return $ s { sideMenuOpen = not (sideMenuOpen s) } -- use a lens!

currentNavPageStore :: ReactStore NavState
currentNavPageStore = mkStore (NavState False Page1)

--------------------------------------------------------------------------
--- History API
--------------------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
    "window['history']['pushState']({page: $1}, $2)"
    js_historyPushState :: Int -> JSString -> IO ()

foreign import javascript unsafe
    "document['title'] = $1;"
    js_setDocTitle :: JSString -> IO ()

foreign import javascript unsafe
    "window['onpopstate'] = function(e) { $1(e['state'] ? e['state'].page : 0); };"
    js_setOnPopState :: Callback (JSVal -> IO ()) -> IO ()
#else
js_historyPushState = undefined
js_setDocTitle = undefined
js_setOnPopState = undefined
#endif

historyPushState :: NavPageId -> IO ()
historyPushState pid = do
    let (_, url) = pageTitleAndUrl pid
    js_historyPushState (fromEnum pid) $ pack url

setDocTitle :: NavPageId -> IO ()
setDocTitle pid = do
    let (title, _) = pageTitleAndUrl pid
    js_setDocTitle $ pack title

initHistory :: IO ()
initHistory = do
    -- set the document title to match page1
    let (title, _) = pageTitleAndUrl Page1
    js_setDocTitle $ pack title

    -- register a callback for onpopstate event
    c <- syncCallback1 ContinueAsync $ \pageRef -> do
        pageInt <- fromMaybe (error "Unable to parse page") <$> fromJSVal pageRef
        alterStore currentNavPageStore $ BackToPage $ toEnum pageInt
    js_setOnPopState c
