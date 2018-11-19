module Lazy where

data Human = Human
  { soname :: String
  , age :: Int
  } deriving Show


dadAndSon :: (Human, Human)
dadAndSon =
  let
    dad = Human "asht" (age son + 28)
    son = Human (soname dad) 27
  in (dad, son)
