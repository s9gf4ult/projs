{-# LANGUAGE BlockArguments #-}

module Main where


newtype It a = It a

g :: ()
g =
  let It be = It id
  in do be do be do ()

main :: IO ()
main = do
  print "kek"
