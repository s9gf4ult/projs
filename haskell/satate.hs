module Main where

import Control.Monad.State
import Control.Monad.Writer
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

emptyleft, emptyright :: (Monad m, Num a) => StateT (Endo ([Operation a]), (a, a)) m ()
emptyleft = StateT $ \(en, (l, r)) -> return((), (mappend en $ Endo (EmptyLeft l:), (0, r)))
                                

emptyright = StateT $ \(en, (l, r)) -> return((), (mappend en $ Endo (EmptyRight r:), (l, 0)))

fillleft, fillright :: (Monad m, Num a, Ord a) => a -> StateT (Endo [Operation a], (a, a)) m ()
fillleft lsize = StateT $ alpha
                 where alpha (en, (l, r)) | l >= lsize = return((), (en, (l, r)))
                                          | otherwise = return((), (mappend en $ Endo (FillLeft (lsize - l):), (lsize, r)))

fillright rsize = StateT $ alpha
                  where alpha (en, (l, r)) | r >= rsize = return((), (en, (l, r)))
                                           | otherwise = return((), (mappend en $ Endo (FillRight (rsize - r):), (l, rsize)))

leftright, rightleft :: (Monad m, Num a, Ord a) => a -> StateT (Endo [Operation a], (a, a)) m ()
leftright rsize = StateT $ black
  where black s@(en, (l, r)) | r >= rsize = return ((), s)
                             | (rsize - r) <= l = return((), (mappend en $ Endo (LeftRight (rsize - r):), (l - (rsize - r), rsize)))
                             | otherwise = return((), (mappend en $ Endo (LeftRight l:), (0, r + l)))

rightleft lsize = StateT $ black
  where black s@(en, (l, r)) | l >= lsize = return ((), s)
                             | (lsize - l) <= r = return((), (mappend en $ Endo (RightLeft (lsize - l):), (lsize, r - (lsize - l))))
                             | otherwise = return((), (mappend en $ Endo (RightLeft r:), (l + r, 0)))

                              
dosomething lsize rsize = do
  fillleft lsize
  leftright  rsize
  fillleft lsize
  fillright rsize
  emptyright
  leftright rsize

-- blah :: (Monad m) => a -> WriterT (Endo [a]) m a
-- blah val = WriterT $ return(val, Endo (val:))

-- someblah a b = do
--   blah a
--   blah b
--   return ()
            
-- inv :: Monad m => StateT [a] m ()
-- inv = do
--   a <- pop
--   b <- pop
--   push a
--   push b
--   return ()