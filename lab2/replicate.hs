--problem 15
replicate :: [a] -> Int -> [a]
replicate xs n = foldl (\acc e -> acc ++ replicate' e n) [] xs
    where
      replicate' _ 0 = []
      replicate' x n = x : replicate' x (n-1)