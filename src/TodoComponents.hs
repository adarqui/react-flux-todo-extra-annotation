{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}



-- | The division between a view and a component is arbitrary, but for me components are pieces that
-- are re-used many times for different purposes.  In the TODO app, there is one component for the
-- text box.
module TodoComponents where



import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Typeable (Typeable)
import           React.Flux



-- | The properties for the text input component.  Note how we can pass anything, including
-- functions, as the properties; the only requirement is an instance of Typeable.
data TextInputArgs = TextInputArgs {
      tiaId          :: Maybe Text
    , tiaClass       :: Text
    , tiaPlaceholder :: Text
    , tiaOnSave      :: T.Text -> [SomeStoreAction]
    , tiaValue       :: Maybe T.Text
} deriving (Typeable)



-- | The text input stateful view.  The state is the text that has been typed into the textbox
-- but not yet saved.  The save is triggered either on enter or blur, which resets the state/content
-- of the text box to the empty string.

-- * newtype ReactView props = ReactView { reactView :: ReactViewRef props }
--
-- * | A stateful-view event handler produces a list of store actions and potentially a new state.  If
-- * -- the new state is nothing, no change is made to the state (which allows an optimization in that we
-- * do not need to re-render the view).
-- *
-- * defineStatefulView :: (Typeable state, NFData state, Typeable props)
-- *                    => JSString -- ^ A name for this view, used only for debugging/console logging
-- *                    -> state -- ^ The initial state
-- *                    -> (state -> props -> ReactElementM (StatefulViewEventHandler state) ()) -- ^ The rendering function
-- *                    -> ReactView props
--
todoTextInput :: ReactView TextInputArgs
todoTextInput =
  defineStatefulView
    "todo text input"  -- * JSString
    ""                 -- * state
                       -- * Text
    $ \curText args -> -- * (state -> props -> ReactElementM (StatefulViewEventHandler state ())
                       -- * (Text -> TextInputArgs -> ReactElementM (StatefulViewEventHandler state())
    input_ $
        maybe [] (\i -> ["id" &= i]) (tiaId args)
        ++
        [ "className" &= tiaClass args
        , "placeholder" &= tiaPlaceholder args
        , "value" &= curText -- using value here creates a controlled component: https://facebook.github.io/react/docs/forms.html
--        , "autoFocus" &= True

        -- Update the current state with the current text in the textbox, sending no actions
        , onChange $ \evt _ -> ([], Just $ target evt "value")

        -- Produce the save action and reset the current state to the empty string
        , onBlur $ \_ _ curState ->
            if not (T.null curState)
                then (tiaOnSave args curState, Just "")
                else ([], Nothing)
        , onKeyDown $ \_ evt curState ->
             if keyCode evt == 13 && not (T.null curState) -- 13 is enter
                 then (tiaOnSave args curState, Just "")
                 else ([], Nothing)
        ]

-- | A combinator suitible for use inside rendering functions.

-- * view :: Typeable props
-- *      => ReactView props -- ^ the view
-- *      -> props -- ^ the properties to pass into the instance of this view
-- *      -> ReactElementM eventHandler a -- ^ The children of the element
-- *      -> ReactElementM eventHandler a
--
todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
todoTextInput_ !args =
  view
    todoTextInput -- * ReactView props
                  -- * ReactView TextInputArgs
    args          -- * props
                  -- * TextInputArgs
    mempty        -- * ReactElementM eventHandler a
                  -- * ReactElementM eventHandler ()
