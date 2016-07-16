{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}



-- | The views for the TODO app
module TodoViews where



import           Control.Monad  (when)
import           React.Flux

import           TodoComponents
import           TodoDispatcher
import           TodoStore



-- | The controller view and also the top level of the TODO app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
--

-- * newtype ReactView props = ReactView { reactView :: ReactViewRef props }
--
-- * newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
--
-- * defineControllerView :: (StoreData storeData, TypeableProps)
-- *                      => Text
-- *                      -> ReactStore storeData
-- *                      -> (storeData -> props -> ReactElementM ViewHandler ())
-- *                      -> ReactView props
--
-- * div_ :: Term eventHandler arg result => arg -> result
--
todoApp :: ReactView ()
todoApp =
  defineControllerView
    "todo app"          -- * Text
    todoStore           -- * ReactStore storeData
                        -- * ReactStore TodoState
    $ \todoState () ->  -- * (storeData -> props -> ReactElementM ViewHandler ())
                        -- * (TodoState -> () -> ReactElementM ViewHandler ())
      div_ $ do
          todoHeader_             -- * ReactElementM eventHandler ()
          mainSection_ todoState  -- * TodoState -> ReactElementM eventHandler ()
          todoFooter_ todoState   -- * TodoState -> ReactElementM eventHandler ()



-- | The TODO header as a React view with no properties.
--

-- * newtype ReactView props = ReactView { reactView :: ReactViewRef props }
--
-- * newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
--
-- * defineView :: Typeable props
-- * => Text
-- * -> (props -> ReactElementM ViewEventHandler ())
-- * -> ReactView props
--
-- * header_ :: Term eventHandler arg result => arg -> result
--
-- * todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
--
-- * dispatchTodo :: TodoAction -> [SomeStoreAction]
--
-- * ($=) :: JSString -> JSString -> PropertyOrHandler handler
--
todoHeader :: ReactView ()
todoHeader =
  defineView
    "header" -- * Text
    $ \() -> -- * (props -> ReactElementM ViewEventHandler ())
             -- * (() -> ReactElementM ViewEventHandler ())
    header_ ["id" $= "header"] $ do
        h1_ "todos"
        todoTextInput_  TextInputArgs
          { tiaId          = Just "new-todo"
          , tiaClass       = "new-todo"
          , tiaPlaceholder = "What needs to be done?"
          , tiaOnSave      = dispatchTodo . TodoCreate
          , tiaValue       = Nothing
          }



-- | A combinator for the header suitable for use inside the 'todoApp' rendering function.

-- * newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
--
-- * view :: Typeable props
-- *      => ReactView props -- ^ the view
-- *      -> props -- ^ the properties to pass into the instance of this view
-- *      -> ReactElementM eventHandler a -- ^ The children of the element
-- *      -> ReactElementM eventHandler a
--
todoHeader_ :: ReactElementM eventHandler ()
todoHeader_ =
  view
    todoHeader -- * ReactView props
               -- * ReactView ()
    ()         -- * props
               -- * ()
    mempty     -- * ReactElementM eventHandler a
               -- * ReactElementM eventHandler ()



-- | A view that does not use a ReactView and is instead just a Haskell function.
-- Note how we use an underscore to signal that this is directly a combinator that can be used
-- inside the rendering function.

-- * onChange :: (Event -> handler) -> PropertyOrHandler handler
--
mainSection_ :: TodoState -> ReactElementM ViewEventHandler ()
mainSection_ st = section_ ["id" $= "main"] $ do
    labeledInput_ "toggle-all" "Mark all as complete"
        [ "type" $= "checkbox"
        , "checked" $= if all (todoComplete . snd) $ todoList st then "checked" else ""
        , onChange $ \_ -> dispatchTodo ToggleAllComplete
        ]

    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st



-- | A view for each todo item.  We specifically use a ReactView here to take advantage of the
-- ability for React to only re-render the todo items that have changed.  Care is taken in the
-- transform function of the store to not change the Haskell object for the pair (Int, Todo), and
-- in this case React will not re-render the todo item.  For more details, see the "Performance"
-- section of the React.Flux documentation.

-- * newtype ReactView props = ReactView { reactView :: ReactViewRef props }
--
-- * defineView :: Typeable props
-- *            => Text
-- *            -> (props -> ReactElementM ViewEventHandler ())
-- *            -> ReactView props
--
-- * (@=) :: A.ToJSON a => JSString -> a -> PropertyOrHandler handler
--
-- * cldiv_ :: JSString -> ReactElementM handler a -> ReactElementM handler a
--
-- * clbutton_ :: JSString  -- ^ class names separated by spaces
-- *           -> handler -- ^ the onClick handler for the button
-- *           -> ReactElementM handler a -- ^ the children
-- *           -> ReactElementM handler a
--
-- * input_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
--
-- * onChange :: (Event -> handler) -> PropertyOrHandler handler
--
-- * onDoubleClick :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
--
-- * elemText :: T.Text -> ReactElementM eventHandler ()
--
todoItem :: ReactView (Int, Todo)
todoItem =
  defineView
    "todo item"           -- * Txt
    $ \(todoIdx, todo) -> -- * (props -> ReactElementM ViewEventHandler ())
    li_ [ classNames [("completed", todoComplete todo), ("editing", todoIsEditing todo)]
        , "key" @= todoIdx
        ] $ do

        cldiv_ "view" $ do
            input_ [ "className" $= "toggle"
                   , "type" $= "checkbox"
                   , "checked" @= todoComplete todo
                   , onChange $ \_ -> dispatchTodo $ TodoSetComplete todoIdx $ not $ todoComplete todo
                   ]

            label_ [ onDoubleClick $ \_ _ -> dispatchTodo $ TodoEdit todoIdx] $
                elemText $ todoText todo

            clbutton_ "destroy" (dispatchTodo $ TodoDelete todoIdx) mempty

        when (todoIsEditing todo) $
            todoTextInput_ TextInputArgs
                { tiaId = Nothing
                , tiaClass = "edit"
                , tiaPlaceholder = ""
                , tiaOnSave = dispatchTodo . UpdateText todoIdx
                , tiaValue = Just $ todoText todo
                }



-- | A combinator for a todo item to use inside rendering functions

-- * -- | Similar to 'viewWithSKey', but with an integer key instead of a string key.
-- * viewWithIKey :: Typeable props
-- *              => ReactView props -- ^ the view
-- *              -> Int -- ^ The key, a value unique within the siblings of this element
-- *              -> props -- ^ The properties to pass to the view instance
-- *              -> ReactElementM eventHandler a -- ^ The children of the view
-- *              -> ReactElementM eventHandler a
--
-- * todoItem :: ReactView (Int, Todo)
--
todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
todoItem_ !todo =
  viewWithIKey
    todoItem   -- * ReactView props
               -- * ReactView (Int, Todo)
    (fst todo) -- * Int
    todo       -- * props
               -- * (Int, Todo)
    mempty     -- * ReactElementM eventHandler a
               -- * ReactElementM eventHandler ()



-- | A view for the footer, taking the entire state as the properties.  This could alternatively
-- been modeled as a controller-view, attaching directly to the store.

-- * newtype ReactView props = ReactView { reactView :: ReactViewRef props }
--
-- * defineView :: Typeable props
-- * => Text
-- * -> (props -> ReactElementM ViewEventHandler ())
-- * -> ReactView props
--
todoFooter :: ReactView TodoState
todoFooter =
  defineView
    "footer"                -- * Text
    $ \(TodoState todos) -> -- * (props -> ReactElementM ViewEventHandler ())
                            -- * (TodoState -> ReactElementM ViewEventHandler ())
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
     in footer_ [ "id" $= "footer"] $ do

            span_ [ "id" $= "todo-count" ] $ do
                strong_ $ elemShow itemsLeft
                elemText $ if itemsLeft == 1 then " item left" else " items left"

            when (completed > 0) $ do
                button_ [ "id" $= "clear-completed"
                        , onClick $ \_ _ -> dispatchTodo ClearCompletedTodos
                        ] $
                    elemString $ "Clear completed (" ++ show completed ++ ")"



-- | A render combinator for the footer

-- * newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
--
-- * view :: Typeable props
-- *      => ReactView props -- ^ the view
-- *      -> props -- ^ the properties to pass into the instance of this view
-- *      -> ReactElementM eventHandler a -- ^ The children of the element
-- *      -> ReactElementM eventHandler a
--
todoFooter_ :: TodoState -> ReactElementM eventHandler ()
todoFooter_ !s =
  view
    todoFooter -- * ReactView props
               -- * ReactView TodoState
    s          -- * props
               -- * TodoState
    mempty     -- * ReactElementM eventHandler a
               -- * ReactElementM eventHandler ()
