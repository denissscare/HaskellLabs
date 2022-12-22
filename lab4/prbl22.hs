module Main where

main::IO() 
main = do putStr (show (range 1 9))

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

iterate':: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

range x y = take' (y-x+1) $ iterate' (+1) x