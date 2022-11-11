--problem 21
insert :: a -> [a] -> Int -> [a]
insert x xs     1 = x:xs
insert x (y:ys) n = y:insert x ys (n-1)