module Main where

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
endotell a = tell $ Endo $ (++ a)

     
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
  put (fillBottle (getFree l) l, r)

fillRight = execSwapped fillLeft

emptyLeft, emptyRight :: (Monad m, Num a) => SolveMonad m a ()
emptyLeft = do
  (l, r) <- get
  put (emptyBottle l, r)
emptyRight = execSwapped emptyLeft

data (Monad m) => CheckStateT s m a = CheckStateT { getSolve :: SolveMonad m s a,
                                                    getChecker :: s -> Bool}

instance (Monad m) => Monad (CheckStateT s m) where
  return a = CheckStateT {getSolve = return a, getChecker = (\_ -> True)}
  (CheckStateT {getSolve = slv, getChecker = chk}) >>= f = 