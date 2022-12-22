data Three a = Empty | Branch a (Three a) (Three a) deriving (Show, Eq)
three = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))(Branch 2 Empty Empty)
main =  do 
  print (count three)  
count :: Three a -> Int
count Empty = 0
count (Branch x Empty Empty) = 1
count (Branch x z c) = count z + count c