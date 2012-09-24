module PostInstance where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad.Trans.Error
import Control.DeepSeq
import Control.Exception
import Common

instance ToRow Storable where
  toRow (Storable a b c d) = [toField a,
                              toField b,
                              toField c,
                              toField d]


instance Storage Connection where
  saveS c s = do
    mapLeftE show $ ErrorT $ try $ executeMany c "insert into storables(a, b, c, d) values (?,?,?,?)" s