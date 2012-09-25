{-# LANGUAGE OverloadedStrings #-}
module PostInstance where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
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

instance FromRow Storable where
  fromRow = do
    a <- field
    b <- field
    c <- field
    d <- field
    return $ Storable a b c d

execLeft :: (Monad m, MonadIO m) => IO a -> ErrorT String m a 
execLeft = (mapLeftE (show :: SomeException -> String)) . ErrorT . liftIO . try

instance Storage Connection where
  saveS c s = do
    execLeft $ executeMany c "insert into storables(a, b, c, d) values (?,?,?,?)" s
    return ()
  getS c = execLeft $ query_ c "select a, b, c, d from storables"
  resetS c = do
    execLeft $ execute_ c "drop table if exists storables"
    execLeft $ execute_ c "create table storables (a integer, b integer, c bigint, d char(100))"
    return ()

  getFilterA (CLT l) c = execLeft $ query c "select a, b, c, d from storables where a < ?" [l]
  getFilterA (CGT r) c = execLeft $ query c "select a, b, c, d from storables where a > ?" [r]
  getFilterA (CLGT l r) c = execLeft $ query c "select a, b, c, d from storables where a > ? and a < ?" (l, r)