module TodoDispatcher (dispatchTodo) where



import           React.Flux
import           TodoStore



-- * data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
-- *    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)
-- *
-- * instance NFData SomeStoreAction where
-- *    rnf (SomeStoreAction _ action) = action `deepseq` ()
--
dispatchTodo :: TodoAction -> [SomeStoreAction]
dispatchTodo a = [SomeStoreAction todoStore a]
