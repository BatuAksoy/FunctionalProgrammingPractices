len :: [a] -> Int
len lst = foldl (+) 0 $ map (\_ -> 1) lst
