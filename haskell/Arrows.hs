
import qualified Control.Category as C

import Control.Arrow

newtype Reader r a b = R ((r, a) -> b)

instance C.Category (Reader r) where
  id = R(\(r, a) -> a)
  (.) (R f) (R g) = R( \(r, a) -> let {b = g (r, a);
                                      c = f (r, b)} in c)

instance Arrow (Reader r) where
  arr f = R(\(r, a) -> f a)
  first (R f) = R(\(r, (a, x)) -> let {b = f (r, a)} in (b, x))


newtype Writer a b = W(a -> (String, b))

instance C.Category Writer where
  id = W(\a -> ("", a))
  (.) (W f) (W g) = W(\a -> let {(s1, b) = g a;
                                (s2, c) = f b} in (s1 ++ s2, c))

instance Arrow Writer where
  arr f = W(\a -> ("", f a))
  first (W f) = W(\(a, x) -> let {(s, b) = f a} in (s, (b, x)))

newtype ListMap a b = LM([a] -> [b])

instance C.Category ListMap where
  id = LM(id)
  (.) (LM f) (LM g) = LM(f . g)

instance Arrow ListMap where
  arr f = LM(map f)
  first (LM f) = LM(\x -> let {(a, b) = unzip x;
                              c = f a} in zip c b)

data Stream a = Cons a (Stream a)

newtype StreamMap a b = SM(Stream a -> Stream b)

instance C.Category StreamMap where
  id = SM(id)
  (SM f) . (SM g) = SM(f . g)

mapStream f (Cons x xs) = Cons (f x) $ mapStream f xs
zipStream (Cons x xs) (Cons y ys) = Cons (x, y) $ zipStream xs ys
unzipStream x = (leftS x, rightS x)
  where
    leftS (Cons (x, _) xs) = Cons x $ leftS xs
    rightS (Cons (_, y) ys) = Cons y $ rightS ys

instance Arrow StreamMap where
  arr f = SM(mapStream f)
  first (SM f) = SM(\x -> let {(a, b) = unzipStream x;
                               c = f a} in zipStream c b )

reshapeStream f (Cons a xs) = case f a of
  Nothing -> reshapeStream f xs
  Just b -> Cons b $ reshapeStream f xs

instance ArrowChoice StreamMap where
  left (SM f) = SM(\s -> rleft s $ f $ reshapeStream getleft s)
    where
      getleft (Right b) = Nothing
      getleft (Left a) = Just a

      rleft :: Stream (Either a b) -> Stream c -> Stream (Either c b)
      rleft (Cons (Right b) xs) y = (Cons (Right b) $ rleft xs y)
      rleft (Cons (Left a) xs) (Cons b ys) = (Cons (Left b) $ rleft xs ys)

newtype Except a x y = E(a x (Either String y))

instance ArrowChoice a => C.Category (Except a) where
  id = E(arr (\x -> Right x))
  (.) (E f) (E g) = undefined -- FFFFFFFFUUUUUUUUUUUUU


trace :: ((a, c) -> (b, c)) -> a -> b
trace f a = let {(b, c) = f (a, c)} in b

instance ArrowLoop StreamMap where
  loop (SM f) = SM(\a -> let {(b, c) = unzipStream $ f $ zipStream a c} in b)
