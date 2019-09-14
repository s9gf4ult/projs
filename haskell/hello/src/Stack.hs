module Stack where

import GHC.Stack

withRankN :: HasCallStack => (HasCallStack => a) -> a
withRankN = id

withoutRankN :: HasCallStack => a -> a
withoutRankN = id
