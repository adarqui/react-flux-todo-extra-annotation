module Main where



import           React.Flux
import           TodoViews



-- * reactRender :: Typeable props => String -> ReactView props -> props -> IO ()
-- * todoApp :: ReactView ()
--
main :: IO ()
main = do
  reactRender
    "todoapp" -- * String
    todoApp   -- * ReactView props
              -- * ReactView ()
    ()        -- * props
              -- * ()
