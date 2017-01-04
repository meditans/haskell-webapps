{-# LANGUAGE OverloadedStrings, TypeApplications #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import MockAPI
import Servant
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Network.Wai.Middleware.Gzip
import Shaped
import Generics.SOP
import Data.Functor.Const

server :: Server MockApi
server = authenticate'' :<|> serveAssets :<|> serveJS
  where
    serveAssets = serveDirectory "../mockClient/assets"
    serveJS = serveDirectory "../mockClient/js/"

authenticate :: (Monad m, MonadIO m) => User -> m Text
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

authenticate' :: (Monad m, MonadIO m) => User -> m (Either (UserShaped (Const (Maybe Text))) User)
authenticate' u
  | correctInfo = do
      liftIO (threadDelay 1000000)
      return (Right u)
  | not userPresent = do
      liftIO (threadDelay 1000000)
      return . Left $ UserShaped (Const $ Just "The user mail is not present") (Const Nothing)
  | otherwise = do
      liftIO (threadDelay 1000000)
      return . Left $ UserShaped (Const Nothing) (Const $ Just "The password is wrong")
  where
    users = M.fromList [ ("user1@gmail.com", "pass1")
                       , ("user2@gmail.com", "pass2")
                       , ("user3@gmail.com", "pass3")
                       ]
    correctInfo = M.lookup (userMail u) users == Just (userPassword u)
    userPresent = userMail u `elem` M.keys users

authenticate'' :: (Monad m, MonadIO m) => User -> m (Either (UserShaped (Const (Maybe Text))) User)
authenticate'' u = case transfGen $ validateRecord u clientValidation of
  Left  a -> return $ Left a
  Right b -> authenticate' b

main :: IO ()
main = run 8081 (gzip gzipSettings $ serve (Proxy @MockApi) server)
  where
    gzipSettings = def { gzipFiles = GzipCompress }

---------------------- Validations

-- serverOnlyValidation :: UserShaped (Validation (Either Text))
-- serverOnlyValidation = UserShaped
--   (Validation . Comp $ \m -> if m `notElem` M.keys userDatabase then Right m else Left "Mail not present in the database")
--   (Validation . Comp $ \p -> if p /= "" then Right p else Left "Blank password")
