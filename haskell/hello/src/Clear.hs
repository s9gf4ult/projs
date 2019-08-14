module Clear where

data ClearT c m a = ClearT
  { monadic :: m a
  , clear   :: c -> c
  }

instance Functor m => Functor (ClearT c m) where
  fmap f (ClearT ma cc) = ClearT (fmap f ma) cc

instance (Applicative m) => Applicative (ClearT c m) where
  pure a = ClearT (pure a) id
  (ClearT mf cc) <*> (ClearT ma cc') = ClearT (mf <*> ma) (cc' . cc)

instance (Applicative m) => Monad (ClearT c m) where
--   (ClearT ma cc) >>= f = ClearT mb ccres
--     where
--       mb = ma >>= f

violate :: (Applicative m) => ClearT a m a -> ClearT a m a
violate ma = do
  a <- ma
  ClearT (pure a) (const a)
