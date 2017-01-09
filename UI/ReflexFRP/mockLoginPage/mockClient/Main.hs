{-# LANGUAGE DataKinds, ExplicitForAll, ExplicitNamespaces, GADTs  #-}
{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction          #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies   #-}
{-# LANGUAGE TypeOperators                                         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import ClassyPrelude

import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex

import Shaped
import Shaped.Reflex

import Data.Functor.Const
import Data.Functor.Misc
import Data.Proxy
import Lens.Micro ((^.), to)

import qualified Language.Javascript.JSaddle.Warp as JSWarp (run)

import MockAPI

main :: IO ()
main = JSWarp.run 8081 $ mainWidget body

--------------------------------------------------------------------------------
-- Implementation of the api endpoints, using servant-reflex

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client (Proxy @MockApi) (Proxy @m) (constDyn url)
  where url :: BaseUrl
        url = BaseFullUrl Http "localhost" 8081 ""

authEndpoint :: (MonadWidget t m) => Endpoint t m User UserShaped
authEndpoint :<|> _ = apiClients

--------------------------------------------------------------------------------
-- Body of the form

body :: forall t m. MonadWidget t m => m ()
body = void . divClass "login-clean" . el "form" $ do
  hiddenTitle
  icon
  (_, serverResponse) <- form userWidget clientValidation "Log In" buttonConfig authEndpoint
  forgotYourUsername
  el "div" $ el "label" (dynText =<< feedback serverResponse)

--------------------------------------------------------------------------------
-- Implementation of the visual elements:

hiddenTitle :: DomBuilder t m => m ()
hiddenTitle = elClass "h2" "sr-only" (text "Login Form")

icon :: DomBuilder t m => m ()
icon = divClass "illustration" (elClass "i" "icon ion-ios-navigate" $ pure ())

mailInputConfig :: Reflex t => TextInputConfig t
mailInputConfig =
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "email" <> "placeholder" =: "Email")
      & textInputConfig_inputType .~ "email"

passInputConfig :: Reflex t => TextInputConfig t
passInputConfig =
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "password" <> "placeholder" =: "Password")
      & textInputConfig_inputType .~ "password"

buttonConfig :: ClassyPrelude.Map AttributeName Text
buttonConfig = "class" =: "btn btn-primary btn-block" <> "type" =: "button"

-- The Shaped widget for inputing the data. As the field are very similar, I use
-- the convenience function userWidgetInternal to abstract the behavior.
userWidget :: (MonadWidget t m) => UserShaped (Formlet t m)
userWidget = UserShaped
  (Formlet $ userWidgetInternal mailInputConfig)
  (Formlet $ userWidgetInternal passInputConfig)

-- Given some configuration, this is a widget that updates the error for the
-- first time on blur (to not annoy the user with validation when it has not
-- finished to type). After the first blur, the error is represented immediately
-- as the field changes.
userWidgetInternal :: MonadWidget t m
                   => TextInputConfig t      -- ^ Special tags for input fields
                   -> Event t ()             -- ^ Event that forces the display of errors
                   -> Dynamic t (Maybe Text) -- ^ Dynamic representing the error
                   -> m (Dynamic t Text)
userWidgetInternal conf upd err = do
  textBox   <- textInput conf
  firstBlur <- headE $ select (textBox ^. textInput_builderElement
                                        . to _inputElement_element
                                        . to _element_events)
                              (WrapArg Blur)
  let b = leftmost [() <$ firstBlur, upd]
  displayedErr <- join <$> holdDyn (constDyn Nothing) (err <$ b)
  el "label" $ dynText (maybe "" id <$> displayedErr)
  return (value textBox)

forgotYourUsername :: DomBuilder t m => m ()
forgotYourUsername = elAttr "a"
  ("href" =: "#" <> "class" =: "forgot")
  (text "Forgot your email or password?")

feedback :: MonadWidget t m
         => Event t (Either Text (Either (UserShaped (Const (Maybe Text))) User))
         -> m (Dynamic t Text)
feedback e = holdDyn "" . ffor e $
  either id
         (either (const "Please fill correctly the informations above")
                 (const "Authenticated"))

