module Main where

import Control.Monad.State


push :: Monad m => a -> StateT [a] m ()
push elem = StateT $ \a -> return((), (elem:a))

pop :: Monad m => StateT [a] m (Maybe a)
pop = StateT $ alpha
      where alpha (x:xs) = return(Just x, xs)
            alpha [] = return(Nothing, [])

-- inv :: Monad m => StateT [a] m ()
-- inv = do
--   a <- pop
--   b <- pop
--   push a
--   push b
--   return ()