{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, DeriveGeneric, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TypeOperators                #-}

module MockAPI where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

-- How can I avoid the need for qualifying things here?
import qualified GHC.Generics as GHC
import           Servant.API

-- Imports due to the shaped approach: reduce these reexporting used things from
-- shaped!
import Control.Monad.Identity
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Const
import Generics.SOP
import Shaped

data User = User
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, GHC.Generic)

instance Generic User

instance ToJSON User
instance FromJSON User

type MockApi = "auth" :> ReqBody '[JSON] User :> Post '[JSON] (Either (UserShaped (Const (Maybe Text))) User)
          :<|> "assets" :> Raw
          :<|> Raw

---------- Validations:

clientValidation :: UserShaped (Validation (Either Text))
clientValidation = UserShaped
  (Validation . Comp $ \m -> if Data.Text.length m > 5 then Right m else Left "Username too short")
  (Validation . Comp $ \p -> if p /= ""                then Right p else Left "Blank password")

---------- Additional declarations needed for the shaped approach (hide these
---------- with TH):

data UserShaped f = UserShaped { userMailLike     :: f Text
                               , userPasswordLike :: f Text}
                  deriving (GHC.Generic)
instance Generic (UserShaped f)

instance ToJSON (UserShaped (Const (Maybe Text)))
instance FromJSON (UserShaped (Const (Maybe Text)))

instance Shaped User UserShaped where
  toShape   (User m p) = UserShaped (Identity m) (Identity p)
  fromShape (UserShaped (Identity m) (Identity p)) = User m p

instance (Show1 f) => Show (UserShaped f) where
  showsPrec d (UserShaped m p) = showParen (d > 10) $
      showString "UserShaped"
    . showChar ' ' . showsPrec1 11 m
    . showChar ' ' . showsPrec1 11 p
