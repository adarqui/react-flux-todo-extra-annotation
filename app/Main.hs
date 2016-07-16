module Main where



import           React.Flux
import           TodoViews



main :: IO ()
main = do
  -- * reactRender :: Typeable props => String -> ReactView props -> props -> IO ()
  -- * todoApp :: ReactView ()
  reactRender
    "todoapp" -- * String
    todoApp   -- * ReactView props
    ()        -- * props
