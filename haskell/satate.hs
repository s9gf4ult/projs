module Main where

import Control.Monad.State
import Data.Monoid

push :: Monad m => a -> StateT [a] m ()
push elem = StateT $ \a -> return((), (elem:a))

pop :: Monad m => StateT [a] m (Maybe a)
pop = StateT $ alpha
      where alpha (x:xs) = return(Just x, xs)
            alpha [] = return(Nothing, [])

data Operation a = FillLeft a
                 | FillRight a
                 | EmptyLeft a
                 | EmptyRight a
                 | LeftRight a
                 | RightLeft a deriving Show

emptyleft, emptyright :: (Monad m, Num a) => StateT (a, a) m (Endo [Operation a])
emptyleft = StateT $ \(l, r) -> return((Endo (EmptyLeft l:)), (0, r))

emptyright = StateT $ \(l, r) -> return((Endo (EmptyRight r:)), (l, 0))

fillleft, fillright :: (Monad m, Num a, Ord a) => a -> StateT (a, a) m (Endo [Operation a])
fillleft lsize = StateT $ alpha
                 where alpha (l, r) | l >= lsize = return(Endo id, (l, r))
                                    | otherwise = return(Endo (FillLeft (lsize - l):), (lsize, r))

fillright rsize = StateT $ alpha
                  where alpha (l, r) | r >= rsize = return(Endo id, (l, r))
                                     | otherwise = return(Endo (FillRight (rsize - r):), (l, rsize))

leftright, rightleft :: (Monad m, Num a, Ord a) => a -> a -> StateT (a, a) m (Endo [Operation a])
leftright lsize rsize = StateT $ black
                        where black (l, r) | r >= rsize = return (Endo id, (l, r))
                                           | (rsize - r) <= l = return(Endo (LeftRight (rsize - r):), (l - (rsize - r), rsize))
                                           | otherwise = return(Endo (LeftRight l:), (0, r + l))

rightleft lsize rsize = StateT $ black
                        where black (l, r) | l >= lsize = return (Endo id, (l, r))
                                           | (lsize - l) <= r = return(Endo (RightLeft (lsize - l):), (lsize, r - (lsize - l)))
                                           | otherwise = return(Endo (RightLeft r:), (l + r, 0))

dosomething lsize rsize = do
  a <- fillleft lsize
  b <- leftright lsize rsize
  return $ mappend a b


            
-- inv :: Monad m => StateT [a] m ()
-- inv = do
--   a <- pop
--   b <- pop
--   push a
--   push b
--   return ()