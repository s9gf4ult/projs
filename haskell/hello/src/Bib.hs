module Bib where

import Data.Generics.Product
import GHC.Generics (Generic)

data Label = Label

data Yoba
  = Yoba { yoba :: Int, boba :: String }
  | Boba { boba :: String }
  deriving (Generic)

instance HasAny 'Label Yoba Yoba String String where
  the = the @"boba"
