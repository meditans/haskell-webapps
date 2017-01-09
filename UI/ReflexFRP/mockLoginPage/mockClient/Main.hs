{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications, DataKinds                                  #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Servant.API
import ClassyPrelude
import Reflex
import Reflex.Dom
import Servant.Reflex
import Lens.Micro ((^.), to)
import qualified Language.Javascript.JSaddle.Warp as JSWarp (run)

import Data.Proxy
import Data.Functor.Misc
import Data.Functor.Const

import Shaped
import Shaped.Reflex
import qualified Generics.SOP as SOP
import Generics.SOP ((:.:)(..), type (-.->)(..), hzipWith, Code, unComp)

import MockAPI

main :: IO ()
main = JSWarp.run 8081 $ mainWidget $ body

--------------------------------------------------------------------------------
-- Implementation of the api endpoints

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

hiddenTitle, icon :: DomBuilder t m => m ()
hiddenTitle = elClass "h2" "sr-only" (text "Login Form")
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

userWidget :: (MonadWidget t m) => UserShaped (Formlet t m)
userWidget = UserShaped
  (Formlet $ userWidgetInternal mailInputConfig)
  (Formlet $ userWidgetInternal passInputConfig)

-- Forms for the shaped approach: This should be supplied by the user, as it's a
-- rendering of the particular markup the user wants for the form.
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

