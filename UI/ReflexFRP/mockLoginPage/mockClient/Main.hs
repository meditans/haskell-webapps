{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications, DataKinds                                  #-}


{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

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

invokeAPI :: forall t m . (MonadWidget t m) => Endpoint t m
invokeAPI :<|> _ = apiClients

--------------------------------------------------------------------------------
-- Body of the form

body :: forall t m. MonadWidget t m => m ()
body = void . divClass "login-clean" . el "form" $ do
  hiddenTitle
  icon
  (_, serverResponse) <- form userWidget clientValidation invokeAPI
  forgotYourUsername
  el "h3" (dynText =<< feedback serverResponse)

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

buttonElement :: DomBuilder t m => Event t () -> Event t () -> m (Event t ())
buttonElement disable enable = divClass "form-group" $ do
  (e, _) <- element "button" conf (text "Log in")
  return (domEvent Click e)
  where
    conf = def & elementConfig_initialAttributes .~ initialAttr
               & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                   [ const disableAttr <$> disable
                   , const enableAttr <$> enable ]
    initialAttr = "class" =: "btn btn-primary btn-block" <> "type" =: "button"
    disableAttr = fmap Just initialAttr  <> "disabled" =: Just "true"
    enableAttr  = fmap Just initialAttr  <> "disabled" =: Nothing

forgotYourUsername :: DomBuilder t m => m ()
forgotYourUsername = elAttr "a"
  ("href" =: "#" <> "class" =: "forgot")
  (text "Forgot your email or password?")

userWidget :: (MonadWidget t m) => UserShaped (FormletSimple t m)
userWidget = UserShaped
  (FormletSimple $ userWidgetInternal mailInputConfig)
  (FormletSimple $ userWidgetInternal passInputConfig)

feedback :: MonadWidget t m
         => Event t (Either Text (Either (UserShaped (Const (Maybe Text))) User))
         -> m (Dynamic t Text)
feedback e = holdDyn "" . ffor e $
  either id
         (either (const "Please fill correctly the informations above")
                 (const "Authenticated"))

-- Forms for the shaped approach: This should be supplied by the user, as it's a
-- rendering of the particular markup the user wants for the form.
userWidgetInternal :: MonadWidget t m => TextInputConfig t -> Dynamic t (Maybe Text) -> m (Dynamic t Text)
userWidgetInternal conf err = do
  textBox   <- textInput conf
  firstBlur <- headE $ select (textBox ^. textInput_builderElement
                                        . to _inputElement_element
                                        . to _element_events)
                              (WrapArg Blur)
  displayedErr <- join <$> holdDyn (constDyn Nothing) (err <$ firstBlur)
  el "error" $ dynText (maybe "" id <$> displayedErr)
  return (value textBox)

--------------------------------------------------------------------------------
-- Things to be moved out of here
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Parse the response from the API. This function could be in servant-reflex
-- (not in reflex-dom). In the meantime we'll keep it in shaped.
parseReqResult :: ReqResult a -> Either Text a
parseReqResult (ResponseSuccess a _) = Right a
parseReqResult (ResponseFailure t _) = Left t
parseReqResult (RequestFailure s)    = Left s

---------------------------------------------------------------------------------

-- A more comprehensive alternative to a form
form :: MonadWidget t m
            => UserShaped (FormletSimple t m)
            -> UserShaped (Validation (Either Text))
            -> Endpoint t m
            -> m ( Dynamic t (Either (UserShaped (Const (Maybe Text))) User)
                 , Event t (Either Text (Either (UserShaped (Const (Maybe Text))) User)) )
form shapedWidget clientVal endpoint = mdo
  -- Here I read a tentative user from the created interface
  rawUser <- createInterface (splitShaped errorEvent) shapedWidget
  -- Here I define the button. This could probably be mixed with the button code
  send <- buttonElement send responseEvent
  -- This part does the server request and parses back the response without
  -- depending on the types in servant-reflex
  serverResponse <- let query = either (const $ Left "Please fill correctly the fields above") Right <$> validationResult
                    in (fmap . fmap) parseReqResult (endpoint query send)
  let
    -- Validation result is the rawUser ran through the validation
    validationResult = transfGen . flip validateRecord clientVal <$> rawUser
    -- Error event is the sum of the event from the form and that of the server
    errorEvent = leftmost [ updated $ either id (const nullError) <$> validationResult
                          , formErrorFromServer ]
    -- Here we retrieve only the error from the server events
    formErrorFromServer = fst . fanEither . snd . fanEither $ serverResponse
    -- This is the event himself, to be used for button disabling
    responseEvent = () <$ serverResponse
  -- In the end, I return both the dynamic containing the event and the raw
  -- event signal from the server. Probably the type could be changed slightly here.
  return (validationResult, serverResponse)

type Endpoint t m = Dynamic t (Either Text User)
                 -> Event t ()
                 -> m (Event t (ReqResult (Either (UserShaped (Const (Maybe Text))) User)))

-- We have to transform a:
-- eResult :: Event t (UserShaped (Const (Maybe Text)))
-- into a
-- nullError' :: UserShaped (Event t :.: Const (Maybe Text))
-- to feed it back recursively to the form.
-- This function seems completely general to me, so should be generalized and taken away
-- TODO: Generalize
-- TODO: Move
splitShaped :: Reflex t => Event t (UserShaped (Const (Maybe Text))) -> UserShaped (Event t :.: Const (Maybe Text))
splitShaped ev = UserShaped
  (Comp $ userMailLike     <$> ev)
  (Comp $ userPasswordLike <$> ev)

-- Temporary name, Form would probably be good too
type Formlet t m = Event t :.: Const (Maybe Text) -.-> m :.: (Dynamic t)

-- This could help with the user facing API
-- type Formlet'' t m a = Dynamic t (Maybe Text) -> m (Dynamic t a)
-- The type approach isn't really workable, as usually because type synonyms
-- cannot be partially applied. So we're left either with constructing newtypes
-- for this, or do only a smart constructor. But, if we want to write the type
-- for the shaped drawingForm, it has to be a newtype.

newtype FormletSimple t m a = FormletSimple (Dynamic t (Maybe Text) -> m (Dynamic t a))

-- This is a generic function, just a way of zipping and sequencing the two parts
createInterface :: forall t m . (MonadWidget t m)
  => UserShaped (Event t :.: Const (Maybe Text))
  -> UserShaped (FormletSimple t m)
  -> m (Dynamic t User)
createInterface shapedError shapedFormlet = unComp . fmap SOP.to . SOP.hsequence $ hzipWith subFun a b
  where
    a :: SOP.POP (Event t :.: Const (Maybe Text)) (Code User)
    a = singleSOPtoPOP . fromSOPI $ SOP.from shapedError
    b :: SOP.SOP (FormletSimple t m) (Code User)
    b = fromSOPI $ SOP.from shapedFormlet

subFun :: MonadWidget t m => (Event t :.: Const (Maybe Text)) a -> FormletSimple t m a -> (m :.: Dynamic t) a
subFun (Comp a) (FormletSimple f) = Comp $ do
  let eventWithoutConst = getConst <$> a
  dynamicError <- holdDyn Nothing eventWithoutConst
  f dynamicError

-- Either move this temporary in shaped with the intent of reporting that
-- upstream, or define a synonym. Discuss this on the generics-sop tracker!
instance (Applicative f, Applicative g) => Applicative (f :.: g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp ((<*>) <$> f <*> x)

-- This could be generated automatically, in fact it's used only in the `form`
-- function, which should be supplied by the library.
nullError :: UserShaped (Const (Maybe Text))
nullError = UserShaped (Const Nothing) (Const Nothing)
