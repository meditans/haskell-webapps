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
import Generics.SOP ((:.:)(..), type (-.->)(..), hzipWith, Code, unComp, fn)

import MockAPI

main :: IO ()
main = do
  -- putStrLn "Server listening on port 8081"
  -- JSWarp.run 8081 $ mainWidget $ void $ textForm mailInputConfig mockValUser
  JSWarp.run 8081 $ mainWidget $ body
  -- JSWarp.run 8081 $ mainWidget $ do
  --   void $ form userWidget clientValidation

body :: forall t m. MonadWidget t m => m ()
body = do
  -- Instructions to use the server at localhost and to invoke the api
  -- Note the usage of ScopedTypeVariables to be able to talk about the monad we're referring to
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _ :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)
  -- A description of the visual elements
  divClass "login-clean" $ do
    el "form" $ do
      rec hiddenTitle
          icon
          user <- form userWidget clientValidation
          let userOk = either (const False) (const True) <$> user
          send <- buttonElement send responseEvent
          forgot
          -- The actual API call
          -- apiResponse <- invokeAPI (Right <$> userResult) send
          -- let responseEvent = const () <$> apiResponse
      -- A visual feedback on authentication
      -- r <- holdDyn "" $ fmap parseR apiResponse
      -- el "h2" (dynText r)
      text "ciao"
  return ()

--------------------------------------------------------------------------------
-- Implementation of the visual elements:

hiddenTitle, icon :: DomBuilder t m => m ()
hiddenTitle = elClass "h2" "sr-only" (text "Login Form")
icon = divClass "illustration" (elClass "i" "icon ion-ios-navigate" $ pure ())

mailInputElement :: MonadWidget t m => m (TextInput t)
mailInputElement = textInput $
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "email" <> "placeholder" =: "Email")
      & textInputConfig_inputType .~ "email"

mailInputConfig :: Reflex t => TextInputConfig t
mailInputConfig =
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "email" <> "placeholder" =: "Email")
      & textInputConfig_inputType .~ "email"

passInputElement :: MonadWidget t m => m (TextInput t)
passInputElement = textInput $
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "password" <> "placeholder" =: "Password")
      & textInputConfig_inputType .~ "password"

passInputConfig :: Reflex t => TextInputConfig t
passInputConfig =
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "password" <> "placeholder" =: "Password")
      & textInputConfig_inputType .~ "password"

buttonElement :: DomBuilder t m => Event t () -> Event t () -> m (Event t ())
buttonElement disable enable = divClass "form-group" (styledButton conf "Log in")
  where
    conf = def & elementConfig_initialAttributes .~ initialAttr
               & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                   [ const disableAttr <$> disable
                   , const enableAttr <$> enable ]
    initialAttr = "class" =: "btn btn-primary btn-block" <> "type" =: "button"
    disableAttr = fmap Just initialAttr  <> "disabled" =: Just "true"
    enableAttr  = fmap Just initialAttr  <> "disabled" =: Nothing

forgot :: DomBuilder t m => m ()
forgot = elAttr "a"
  ("href" =: "#" <> "class" =: "forgot")
  (text "Forgot your email or password?")

----- This function should be contributed back to reflex-frp
styledButton :: DomBuilder t m => ElementConfig EventResult t m -> Text -> m (Event t ())
styledButton conf t = do
  (e, _) <- element "button" conf (text t)
  return (domEvent Click e)

--------------------------------------------------------------------------------
-- Parse the response from the API
parseR :: ReqResult Text -> Text
parseR (ResponseSuccess a _) = a
parseR (ResponseFailure a _) = "ResponseFailure: " <> a
parseR (RequestFailure s)    = "RequestFailure: " <> s

---------------------------------------------------------------------------------
-- Quello che mi serve adesso e' un modo per trasformare un UserShaped fatto di
-- tanti `m (Dynamic t (Either Text Text))`. In particolare dev'essere una cosa
-- come sequence praticamente.

-- Devo adesso aggregare le validazioni in un'unica validazione
-- UserShaped (Either Text)

-- Ho anche bisogno di qualcosa che contenga i moduli

form :: MonadWidget t m
     => UserShaped (Formlet2 t m)
     -> UserShaped (Validation (Either Text))
     -> m (Dynamic t (Either (UserShaped (Const (Maybe Text))) User))
form uf uv = mdo
  tentative <- experiment (splitShaped errorEvent) uf
  let validationResult = traceDyn "validationResult: " $ transfGen . flip validateRecord uv <$> tentative
      errorEvent = updated $ either id (const nullError) <$> validationResult
  return validationResult

-- We have to transform a:
-- eResult :: Event t (UserShaped (Const (Maybe Text)))
-- into a
-- nullError' :: UserShaped (Event t :.: Const (Maybe Text))
-- to feed it back recursively to the form.
splitShaped :: Reflex t => Event t (UserShaped (Const (Maybe Text))) -> UserShaped (Event t :.: Const (Maybe Text))
splitShaped ev = UserShaped
  (Comp $ userMailLike     <$> ev)
  (Comp $ userPasswordLike <$> ev)

-- Temporary name
type Formlet2 t m = Event t :.: Const (Maybe Text) -.-> m :.: (Dynamic t)

-- Questo non deve occuparsi di validazione: deve semplicemente disegnare il
-- form e restituire l'utente candidato, ancora da validare.
experiment :: forall t m . (MonadWidget t m)
  => UserShaped (Event t :.: Const (Maybe Text))
  -> UserShaped (Formlet2 t m)
  -> m (Dynamic t User)
experiment shapedError shapedFormlet = unComp . fmap SOP.to . SOP.hsequence $ hzipWith subFun a b
  where
    a :: SOP.POP (Event t :.: Const (Maybe Text)) (Code User)
    a = singleSOPtoPOP . fromSOPI $ SOP.from shapedError
    b :: SOP.SOP (Formlet2 t m) (Code User)
    b = fromSOPI $ SOP.from shapedFormlet

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp ((<*>) <$> f <*> x)

subFun :: (Event t :.: Const (Maybe Text)) a -> Formlet2 t m a -> (m :.: Dynamic t) a
subFun a (Fn f) = f a

userWidget :: (MonadWidget t m) => UserShaped (Formlet2 t m)
userWidget = UserShaped
  (fn $ \(Comp e) -> Comp $ do
      let unwrappedError = getConst <$> e
      dynamicError <- holdDyn Nothing unwrappedError
      userWidgetInternal mailInputConfig dynamicError)
  (fn $ \(Comp e) -> Comp $ do
      let unwrappedError = getConst <$> e
      dynamicError <- holdDyn Nothing unwrappedError
      userWidgetInternal passInputConfig dynamicError)

nullError :: UserShaped (Const (Maybe Text))
nullError = UserShaped (Const Nothing) (Const Nothing)

-- type Formlet2 t m = Event t :.: Const (Maybe Text) -.-> m :.: (Dynamic t)
-- Forms for the shaped approach:

textForm :: MonadWidget t m => TextInputConfig t -> (Text -> Maybe Text) -> m (Dynamic t Text)
textForm conf val = do
  textBox   <- textInput conf
  firstBlur <- headE $ select (textBox ^. textInput_builderElement
                                        . to _inputElement_element
                                        . to _element_events)
                              (WrapArg Blur)
  err <- join <$> holdDyn (constDyn Nothing)
                          ((val <$> value textBox) <$ firstBlur)
  -- Let's represent the error
  el "h4" $ dynText (maybe "" id <$> err)
  return (value textBox)


userWidgetInternal :: MonadWidget t m => TextInputConfig t -> Dynamic t (Maybe Text) -> m (Dynamic t Text)
userWidgetInternal conf err = do
  textBox   <- textInput conf
  firstBlur <- headE $ select (textBox ^. textInput_builderElement
                                        . to _inputElement_element
                                        . to _element_events)
                              (WrapArg Blur)
  displayedErr <- join <$> holdDyn (constDyn Nothing)
                          (err <$ firstBlur)
  -- Let's represent the error
  el "h4" $ dynText (maybe "" id <$> displayedErr)
  return (value textBox)
