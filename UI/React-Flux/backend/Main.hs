{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import Common.API
import Common.UserLogin

import Servant
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Network.Wai.Middleware.Gzip

server :: Server Api
server = authenticate :<|> serveAssets
  where
    serveAssets = serveDirectory "../frontend"

authenticate :: (MonadIO m) => User -> m Text
authenticate u
  | correctInfo = liftIO (threadDelay 1000000) >> return "Authenticated"
  | userPresent = liftIO (threadDelay 1000000) >> return "Wrong password"
  | otherwise   = liftIO (threadDelay 1000000) >> return "Not Authenticated"
  where
    users = M.fromList [ ("user1@gmail.com", "pass1")
                       , ("user2@gmail.com", "pass2")
                       , ("user3@gmail.com", "pass3")
                       ]
    correctInfo = M.lookup (userMail u) users == Just (userPassword u)
    userPresent = userMail u `elem` M.keys users

main :: IO ()
main = run 8081 (gzip gzipSettings $ serve (Proxy @Api) server)
  where
    gzipSettings = def { gzipFiles = GzipCompress }
