module Main where

import Control.Monad (unless, when)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.RWS
import Data.Monoid
import Control.Arrow ((&&&))

data Bottle a = Bottle {getSize :: a,
                        getMax :: a} deriving Show

getFree :: (Num a) => Bottle a -> a
getFree b = (getMax b) - (getSize b)

emptyBottle :: (Num a) => Bottle a -> Bottle a
emptyBottle (Bottle {getSize = _, getMax = m}) = Bottle {getSize = 0, getMax = m}

fillBottle :: (Ord a, Num a) => a -> Bottle a -> Bottle a
fillBottle am (Bottle {getSize = s, getMax = m}) = Bottle {getSize = max 0 $ min m $ am + s, getMax = m}

data Action a = Flow {getFrom :: Bottle a,
                      getTo :: Bottle a,
                      getAmount :: a}
              | Fill {getWhat :: Bottle a,
                      getAmount :: a}
              | Empty {getWhat :: Bottle a,
                       getAmount :: a} deriving Show
                      

type SolveMonad m a ret = RWST () (Endo [Action a]) (Bottle a, Bottle a) m ret

endotell :: (Monad m) => [Action a] -> SolveMonad m a ()
endotell a = tell $ Endo $ (a ++)

     
leftToRight, rightToLeft :: (Monad m, Num a, Ord a) => SolveMonad m a ()
leftToRight = do
  (l, r) <- get
  if (getFree r) >= (getSize l)
    then do endotell [(Flow {getFrom = l, getTo = r, getAmount = (getSize l)})]
            put (emptyBottle l, fillBottle (getSize l) r)
    else do endotell [(Flow {getFrom = l, getTo = r, getAmount = (getFree r)})]
            put (fillBottle (negate $ getFree r) l, fillBottle (getSize l) r)

execSwapped :: (Monad m) => SolveMonad m a () -> SolveMonad m a ()
execSwapped mex = do
  modify (snd &&& fst)
  mex
  modify (snd &&& fst)

rightToLeft = execSwapped leftToRight

fillLeft, fillRight :: (Monad m, Num a, Ord a) => SolveMonad m a ()
fillLeft = do
  (l, r) <- get
  endotell [(Fill {getWhat = l, getAmount = (getFree l)})]
  put (fillBottle (getFree l) l, r)

fillRight = execSwapped fillLeft

emptyLeft, emptyRight :: (Monad m, Num a) => SolveMonad m a ()
emptyLeft = do
  (l, r) <- get
  endotell [(Empty {getWhat =l, getAmount = (getSize l)})]
  put (emptyBottle l, r)
emptyRight = execSwapped emptyLeft

checkState :: (Eq a) => a -> (Bottle a, Bottle a) -> Bool
checkState sz (Bottle s1 _, Bottle s2 _) = s1 == sz || s2 == sz

isFull :: (Ord a) => Bottle a -> Bool
isFull (Bottle s m) = s >= m
                      
solve :: (Monad m, Eq a, Num a, Ord a) => a -> SolveMonad m a ()
solve sz = do
  s <- get
  unless (checkState sz s) $ do
    fillLeft
    s1 <- get
    unless (checkState sz s1) $ do
      leftToRight
      s2@(l, r) <- get
      unless (checkState sz s2) $ do
        when (isFull r) $ do
          emptyRight
          leftToRight
        solve sz

-- data (MonadState s m) => StateCheck s m a =
--   StateCheck { runStateCheck :: m a,
--                getCheckFunction :: s -> Bool}
--   | StateStop

-- instance (MonadState s m) => Monad (StateCheck s m) where
--   return a = StateCheck { runStateCheck = ret,
--                           getCheckFunction = \_ -> False}
--     where
--       ret = return a
--   StateStop >>= _ = StateStop
--   m@StateCheck {runStateCheck = inner,
--                 getCheckFunction = chkfn} >>= f = StateCheck $ do
--     s <- get
--     if (chkfn s)
--       then return StateStop
--       else return $ StateCheck {runStateCheck = 