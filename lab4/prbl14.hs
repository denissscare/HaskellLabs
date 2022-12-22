main = do
    print . dupli $ "abc"
    print . dupli $ [1, 2, 3, 4, 5, 67]

dupli  :: [a] -> [a]
dupli = foldr' (\ x xs -> x : x : xs) []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)  