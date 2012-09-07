{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Data.Text as T
import Text.Templating.Heist
import Text.XmlHtml as X

main = do
  a <- runErrorT $ do
    state <- ErrorT $ loadTemplates "templates"
             $ bindSplices [("mySplice", mySplice)]  defaultHeistState
    tmpl <- lift $ renderTemplate state "default"
    maybe (throwError "Could not render template") (return . toByteString . fst) tmpl
  either putStrLn print a

mySplice :: (Monad m, MonadIO m) => Splice m
mySplice = do
  s <- liftIO getLine 
  return [Element "div" [] [TextNode $ pack s]]