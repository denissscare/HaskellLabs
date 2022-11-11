{-# LANGUAGE MultiWayIf #-}


data Root = TwoSolution Float Float | OneSolution Float | NoSolution deriving Show

rootFunction::Float -> Float -> Float -> Root
rootFunction a b c =
    let d = (b^2 - 4 * a * c) in
        if| d > 0 ->(TwoSolution(((-b) + (sqrt(d))/2*a)) ((-b)-(sqrt(d))/2*a))
          | d < 0 -> NoSolution
          | otherwise -> OneSolution((-b)/2*a)