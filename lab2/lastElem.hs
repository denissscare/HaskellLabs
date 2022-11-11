--problem 1
lastElem :: [e] -> e
lastElem [] = error "empty list"
lastElem [x] = x
lastElem (_:xs) = lastElem xs
