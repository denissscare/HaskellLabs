data BinarySearchTree = Null | Node Int (BinarySearchTree) (BinarySearchTree)
  deriving Show
ins Null val = Node val Null Null
ins (Node x lft rht) val
  | x > val = Node x left' rht
  | otherwise = Node x lft right'
  where
  left' = ins lft val
  right' = ins rht val
fromList xs = foldl ins Null xs
arr Null = []
arr (Node x lft rht) = arr lft ++ [x] ++ arr rht
main = do
  print $ tr
  print $ tr
  print $ arr tr1
  where
  a = ins Null 7
  b = ins a 3
  tr = ins b 9
  tr1 = fromList [4,1,2,3,45,6,2,4]