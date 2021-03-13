module Leet897 where

import           Data.Foldable

data TreeNode a = TreeNode a (Maybe (TreeNode a)) (Maybe (TreeNode a))
  deriving (Eq, Ord, Show)

walkTree :: TreeNode a -> [a]
walkTree (TreeNode a l r) =
  (toList l >>= walkTree) ++ [a] ++ (toList r >>= walkTree)

toTreeNode :: [a] -> TreeNode a
toTreeNode (a:[])   = TreeNode a Nothing Nothing
toTreeNode (a:rest) = TreeNode a Nothing (Just $ toTreeNode rest)
