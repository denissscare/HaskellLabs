--problem 14
myDuplicate [] = []
myDuplicate (x:xs) = x:x:myDuplicate xs