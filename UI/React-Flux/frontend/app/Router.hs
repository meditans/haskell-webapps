{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}

#ifdef __GHCJS__
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif

module Router (setLocationHash
              , getLocationHash
              , onLocationHashChange
              , actionRoute
              , childRoutePath
              , initRouter
              , storeRouter
              ) where

import React.Flux

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC
import qualified Data.Text              as T
import qualified Web.Routes             as WR


#ifdef __GHCJS__

import           Control.Monad          (liftM)
import qualified Data.JSString          as JSS
import           GHCJS.Foreign.Callback
import           GHCJS.Types            (JSString, JSVal)
import           Unsafe.Coerce

#endif

--------------------------------------------------------------------------------
-- Low level functions, to act on the browser bar when we're in ghcjs
--------------------------------------------------------------------------------

#ifdef __GHCJS__

foreign import javascript unsafe
  "window.onhashchange = function() {$1(location.hash.toString());}"
  js_attachtLocationHashCb :: (Callback (JSVal -> IO ())) -> IO ()

foreign import javascript unsafe
  "window.location.hash = $1"
  js_setLocationHash :: JSString -> IO ()

foreign import javascript unsafe
  "window.location.hash.toString()"
  js_getLocationHash :: IO JSString

setLocationHash :: String -> IO ()
setLocationHash = js_setLocationHash . JSS.pack

getLocationHash :: IO (Maybe String)
getLocationHash = do
  rt <- liftM JSS.unpack js_getLocationHash
  return $ case rt of
    "" -> Nothing
    _  -> Just rt

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange fn = do
  cb <- syncCallback1 ThrowWouldBlock (fn . JSS.unpack . unsafeCoerce)
  js_attachtLocationHashCb cb

# else

setLocationHash :: String -> IO ()
setLocationHash _ = return ()

getLocationHash :: IO (Maybe String)
getLocationHash = return Nothing

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange _ = return ()

#endif

--------------------------------------------------------------------------------
-- Higher level functions, using Web Routes to construct routing from a store
-- which has an instance of PathInfo for the StoreAction.
--------------------------------------------------------------------------------

childRoutePath :: WR.PathInfo action => action -> [T.Text]
childRoutePath = WR.toPathSegments

actionRoute :: WR.PathInfo action => Maybe ([T.Text] -> T.Text) -> action -> T.Text
actionRoute mparentRouter action =
  frag
  where
    path = maybe (WR.toPathInfo action) ($ childRoutePath action) mparentRouter
    frag = if "#" `T.isPrefixOf` path
           then path
           else T.cons '#' path

initRouter :: ([T.Text] -> IO ()) -> IO ()
initRouter router =
  onLocationHashChange $ router . stripHash . WR.decodePathInfo . BC.pack
  where
    stripHash ("#":path) = path
    stripHash path       = path

storeRouter :: (StoreData store, WR.PathInfo (StoreAction store)) =>
               ReactStore store -> [T.Text] -> IO ()
storeRouter store rt = either (const $ return ()) id $ WR.runSite "" site rt
  where
    site = WR.mkSitePI $ WR.runRouteT $ routerAlterStore
    routerAlterStore action = liftIO (alterStore store action)
