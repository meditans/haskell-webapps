{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Frontend.Auth.Components where

import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import           React.Flux
import           React.Flux.Internal (JSString)

-- | The properties for the text input component.  Note how we can pass anything, including
-- functions, as the properties; the only requirement is an instance of Typeable.
data TextInputArgs = TextInputArgs {
      tiaId          :: Maybe JSString
    , tiaClass       :: JSString
    , tiaPlaceholder :: JSString
    , tiaType        :: JSString
    , tiaOnSave      :: T.Text -> [SomeStoreAction]
    , tiaValue       :: Maybe T.Text
    } deriving (Typeable)

-- | The text input stateful view.  The state is the text that has been typed into the textbox
-- but not yet saved.  The save is triggered either on enter or blur, which resets the state/content
-- of the text box to the empty string.
textInput :: ReactView TextInputArgs
textInput = defineStatefulView "todo text input" "" $ \curText args ->
    input_ $
        maybe [] (\i -> ["id" &= i]) (tiaId args)
        ++
        [ "className"   &= tiaClass args
        , "placeholder" &= tiaPlaceholder args
        , "type"  &= tiaType args
        , "value" &= curText -- using value here creates a controlled component: https://facebook.github.io/react/docs/forms.html

        -- Update the current state with the current text in the textbox, sending no actions
        , onChange $ \evt _ -> ([], Just $ target evt "value")

        -- Produce the save action and reset the current state to the empty string
        , onBlur $ \_ _ curState ->
            -- if not (T.null curState)
            --     then (tiaOnSave args curState, Nothing)
            --     else ([], Nothing)
            (tiaOnSave args curState, Nothing)

        , onKeyDown $ \_ evt curState ->
             if keyCode evt == 13 && not (T.null curState) -- 13 is enter
                 then (tiaOnSave args curState, Nothing)
                 else ([], Nothing)
        ]

-- | A combinator suitible for use inside rendering functions.
textInput_ :: TextInputArgs -> ReactElementM eventHandler ()
textInput_ !args = view textInput args mempty
