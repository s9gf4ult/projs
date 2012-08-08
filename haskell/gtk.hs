{-# LANGUAGE ExistentialQuantification #-}

import Graphics.UI.Gtk
import System.Glib.Signals
import Data.Either
import Data.Maybe

data W = forall w. WidgetClass w => W w

data Hernia = Hernia Integer
            | Mutota Integer
            | Something
              deriving (Show, Read)

instance Num Hernia where
  (Hernia x) + (Hernia y) = Hernia (x + y)
  (Mutota x) + (Mutota y) = Mutota (x + y)
  (Hernia x) + (Mutota y) = Hernia (x + y*10)
  (Mutota x) + (Hernia y) = (Hernia y) + (Mutota x)
  Something + _ = Something
  _ + Something = Something
  negate (Hernia x) = Hernia (- x)
  negate (Mutota x) = Mutota (- x)
  negate Something = Something
  (Hernia x) * (Hernia y) = Hernia (x * y)
  (Mutota x) * (Mutota y) = Mutota (x * y)
  (Hernia x) * (Mutota y) = Mutota (y * x * 10)
  (Mutota x) * (Hernia y) = (Hernia y) * (Mutota x)
  Something * _ = Something
  _ * Something = Something
  abs (Hernia x) = Hernia (abs x)
  abs (Mutota x) = Mutota (abs x)
  abs Something = Something
  signum (Hernia x) = Hernia (signum x)
  signum (Mutota x) = Mutota (signum x)
  signum Something = Something
  fromInteger x = Hernia x
  
              
  
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
  on button buttonActivated $ clickfunc e1 e2 lout
  on e1 editableChanged $ clickfunc e1 e2 lout
  on e2 editableChanged $ clickfunc e1 e2 lout
  
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
  Right s -> show (s :: Hernia)

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