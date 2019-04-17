mapReverse :: [Int] -> [Int] -> [Int]
mapReverse xs acc = case xs of
                []      -> acc
                x:xs'   -> mapReverse xs' (x:acc)

rev l = mapReverse l []
