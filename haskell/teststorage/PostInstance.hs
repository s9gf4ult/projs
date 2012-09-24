{-# LANGUAGE OverloadedStrings #-}
module PostInstance where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad.Trans.Error
import Control.DeepSeq
import Control.Exception
import Common
import Control.Monad.IO.Class

instance ToRow Storable where
  toRow (Storable a b c d) = [toField a,
                              toField b,
                              toField c,
                              toField d]

execLeft :: (Monad m, MonadIO m) => IO a -> ErrorT String m a 
execLeft = (mapLeftE (show :: SomeException -> String)) . ErrorT . liftIO . try

instance Storage Connection where
  saveS c s = do
    execLeft $ executeMany c "insert into storables(a, b, c, d) values (?,?,?,?)" s
    return ()
  getS c = execLeft $ query_ c "select a, b, c, d from storables"