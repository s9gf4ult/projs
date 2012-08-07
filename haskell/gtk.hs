{-# LANGUAGE ExistentialQuantification #-}

import Graphics.UI.Gtk
import Data.Either
import Data.Maybe

data W = forall w. WidgetClass w => W w
  
main :: IO ()
main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit 
  root <- makeRoot
  containerAdd window root
  widgetShowAll window
  mainGUI

makeRoot = do
  e1 <- entryNew
  e2 <- entryNew
  lplus <- labelNew $ Just "+"
  lout <- labelNew Nothing
  h <- withHBox [W e1, W lplus, W e2, W lout]
  button <- buttonNew
  onClicked button $ clickfunc e1 e2 lout
  set button [buttonLabel := "Compute"]
  v <- withVBox [W h, W button]
  return v

clickfunc e1 e2 lout = do
  t1 <- entryGetText e1
  t2 <- entryGetText e2
  labelSetText lout $ calculate t1 t2

calculate :: String -> String -> String
calculate t1 t2 = case (calc t1 t2) of
  Left s -> s
  Right s -> show (s :: Integer)

calc :: (Read x, Num x) => String -> String -> Either String x
calc t1 t2 = do
  a <- readString "Argument 1: " t1
  b <- readString "Argument 2: " t2
  return (a + b)

readString :: (Read x) => String -> String -> Either String x
readString message str = maybeToEither (message ++ "could not read value" ++ (show str)) $ maybeRead str

maybeRead :: (Read x) => String -> Maybe x
maybeRead str = do
  (x, str) <- listToMaybe $ reads str
  if str == ""
    then return x
    else Nothing

maybeToEither mess Nothing = Left mess
maybeToEither _ (Just x) = Right x
  

withHBox :: [W] -> IO HBox
withHBox x = do
  h <- hBoxNew False 0
  withBox h x

withVBox :: [W] -> IO VBox
withVBox x = do
  v <- vBoxNew False 0
  withBox v x

withBox :: (BoxClass box) => box -> [W] -> IO box
withBox box x = do
  mapM_ specpack x
  return box
    where
      specpack (W w) = boxPackStart box w PackNatural 0